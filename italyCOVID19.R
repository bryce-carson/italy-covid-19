library(tidyverse)
library(magrittr)
library(COVID19)
library(deSolve)
library(optimx)
library(countrycode)
library(gridExtra)

## Used to cache function results from covid19, so data is only downloaded once
## without needing to assign it to an object ourselves.
library(memoise)

countryNamesInEnglish <- unique(countryname_dict[, 1])

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
  if (!country %in% countryNamesInEnglish)
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

print(prettyNum(regionalPopulation, big.mark = ","))

  as_tibble(regionalCOVID19Data) %>%
    select(date, confirmed, deaths, recovered) %>%
    rename(dead = deaths) %>%
    mutate(infections = c(0, diff(confirmed)),
           recoveries = c(0, diff(recovered)),
           deaths = c(0, diff(dead)),
           prevalence = confirmed - recovered - dead) %>%
    # Correct the prevalence column based on "the formula".
    mutate(#prevalence = lag(prevalence) + infections - recoveries - deaths,
      susceptible = mapply(sum,
                           regionalPopulation,
                           -infections,
                           -recoveries,
                           -deaths),
      .keep = "all") %>%
    slice(-1)
}

observed <- covid19.regional() %>%
  select(susceptible, confirmed, recovered, dead, prevalence, deaths, recoveries, date)

observed

# Define the SIRD model
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

# Initial state values
init <- c(S = observed$susceptible[1], I = observed$prevalence[1], R = observed$recovered[1], D = observed$dead[1])

# Time vector
times <- seq(1, nrow(observed), by = 1)

# Objective function to minimize the residual sum of squares (RSS)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma", "delta")
  out <- lsoda(y = init, times = times, func = SIRD, parms = parameters)
  sum((observed$susceptible - out[, "S"])^2 +
        (observed$prevalence - out[, "I"])^2 +
        (observed$recovered - out[, "R"])^2 +
        (observed$dead - out[, "D"])^2)
}

# Initial guesses for parameters
initial_guesses <- list(
  c(beta = 0.4, gamma = 0.1, delta = 0.01),
  c(beta = 0.3, gamma = 0.1, delta = 0.02),
  c(beta = 0.5, gamma = 0.1, delta = 0.03)
)

fit_optimization <- function(init_params) {
  fit <- tryCatch(
    {
      optim(par = init_params, fn = RSS, method = "L-BFGS-B", lower = c(0, 0, 0), upper = c(1, 1, 1))
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

opt_params <- NULL
for (guess in initial_guesses) {
  opt_params <- fit_optimization(guess)
  if (!is.null(opt_params)) break
}

if (is.null(opt_params)) {
  stop("Optimization did not converge with any initial guesses.")
}

# Print the best-fit parameters
opt_params

#Returns a delta of 0.00000000

# Solve the SIRD model with the optimized parameters
out <- lsoda(y = init, times = times, func = SIRD, parms = opt_params)

out

# Convert the output to a data frame for plotting
output <- as.data.frame(out)
output$date <- observed$date

# Plot the results
observed_long <- observed %>% 
  select(-susceptible, -confirmed) %>% 
  pivot_longer(cols = -date, names_to = "compartment", values_to = "observed")

output_long <- output %>%
  select(-S) %>%
  pivot_longer(cols = -date, names_to = "compartment", values_to = "predicted")

plot_data <- full_join(observed_long, output_long, by = c("date", "compartment"))

ggplot(plot_data, aes(x = date)) +
  geom_line(aes(y = observed, color = compartment), linewidth = 1) +
  geom_line(aes(y = predicted, color = compartment), linetype = "dashed", linewidth = 1) +
  labs(title = "SIRD Model Fit to COVID-19 Data",
       x = "Date",
       y = "Count",
       color = "Compartment") +
  theme_minimal()
