## https://covid19datahub.io/articles/docs.html#epidemiological-variables
##
## All variables are cumulative except hosp, icu, vent, and population;
## population is the total population as of a census date, and does not change,
## the prior variables are the number of patients on that date in that
## category.
covid19 <- memoise(COVID19::covid19)

covid19.regional <- function(country = "Italy",
                             subregion = "Lombardia",
                             startDate = as.Date("2020-08-31"),
                             endDate = as.Date("2020-11-30")) {
  ## Ensure the country named in the argument is named in English.
  if (!country %in% unique(countryname_dict[, 1]))
    errorCondition(paste("Argument `country` was not given in English,",
                         "or is not a recognized country."))
  
  message("`country` was valid; assuming all other arguments are valid.")
  
  regionalCOVID19Data <-
    covid19(country, 2, startDate, endDate, verbose = FALSE) %>%
    select(date,
           confirmed,
           deaths,
           recovered,
           administrative_area_level_2,
           key_gadm,
           population) %>%
    rename(
      region = administrative_area_level_2,
      GADM = key_gadm
    ) %>%
    filter(region == subregion) %>%
    mutate(date = as.Date(date)) %>%
    arrange(date)
  
  if(nrow(regionalCOVID19Data) == 0) {
    errorCondition("No data available for selected region.")
  }
  
  regionalPopulation <- first(regionalCOVID19Data$population)
  
  message(sprintf(r"[Spatial information:
  Country = %s
  Region = %s
  GADM = %s
  Region population = %s]",
  country,
  subregion,
  first(regionalCOVID19Data$GADM),
  prettyNum(regionalPopulation, big.mark = ",")))

  as_tibble(regionalCOVID19Data) %>%
    select(date, confirmed, deaths, recovered) %>%
    rename(dead = deaths) %>%
    mutate(newCases = c(0, diff(confirmed)),
           newRecovered = c(0, diff(recovered)),
           newDead = c(0, diff(dead)),
           prevalence = confirmed - recovered - dead) %>%
    # Correct the prevalence column based on "the formula".
    mutate(prevalence = lag(prevalence) + newCases - newRecovered - newDead,
           susceptible = mapply(sum,
                                regionalPopulation,
                                -prevalence,
                                -recovered,
                                -dead),
           .keep = "all") %>%
    slice(-1) %>% # drop the first row.
    select(date,
           susceptible, confirmed, recovered, dead,
           prevalence, newCases, newDead, newRecovered)
}

# Define the mean field SIRD model
SIRD <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I - delta * I
    dR <- gamma * I
    dD <- delta * I
    list(c(dS, dI, dR, dD))
  })
}

# Objective function to minimize the residual sum of squares (RSS)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma", "delta")
  out <- lsoda(y = initialStateValues, times = 1:365, func = SIRD, parms = parameters)
  sum(#(observed$susceptible - out[, "S"])^2 +
        (observed$prevalence - out[, "I"])^2 +
        (observed$recovered - out[, "R"])^2 +
        (observed$dead - out[, "D"])^2)
}

fit_optimization <- function(init_params) {
  fit <- tryCatch(
    {
      #optim(par = init_params, fn = RSS, method = "L-BFGS-B", lower = c(0, 0, 0), upper = c(1, 1, 1))
      optim(par = init_params, fn = RSS, method = "L-BFGS-B", lower = lower_bounds, upper = upper_bounds)
    },
    error = function(e) {
      message("Optimization error: ", e)
      NULL
    }
  )
  
  if (!is.null(fit) && fit$convergence == 0) {
    return(fit$par)
  } else {
    return(NULL)
  }
}