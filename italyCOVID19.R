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

## Define the function to solve the SIRD model.
sird <- function(t = seq(0, 90),
                 ## Cumulative value of the compartment at the outset of the
                 ## simulation.
                 susceptible = 499,
                 prevalence = 1,
                 confirmed = 1,
                 recovered = 0,
                 dead = 0,
                 deaths = 0,
                 recoveries = 0,

                 ## Parameters
                 beta = 5e-3,
                 gamma = 1e-3,
                 delta = 3e-4) {
  lsoda(
    c(
      S = susceptible,
      I = prevalence, #confirmed,
      R = recovered,
      D = dead
    ),
    t,
    function(t, y, parms, ...) {
      ## lsoda's documentation is out of date, as the names of y are not
      ## available inside func, so we construct an environment from some data
      ## ourselves. ***This is the slowest part of this function, taking ~130ms
      ## while the expression evaluated inside this data environment only takes
      ## ~80ms.*** It is commented for understandability, and the optimized,
      ## less readable version is preferred for speed.
      # with(as.list(c(y, parms)), {
      #   N <- S + I + R
      #   βSIN <- beta * S * I / N
      #   γI <- gamma * I
      #   δI <- delta * I
      #   list(c(-βSIN,          # dS/dt
      #          βSIN - γI - δI, # dI/dt
      #          γI,             # dR/dt
      #          δI),            # dD/dt
      #        population = N)
      # })
      N <- y[1] + y[2] + y[3]
      βSIN <- parms[1] * y[1] * y[2] / N
      γI <- parms[2] * y[2]
      δI <- parms[3] * y[2]
      list(c(-βSIN, # dS/dt
             βSIN - γI - δI, # dI/dt
             γI, # dR/dt
             δI), # dD/dt
           "population" = N)
    },
    parms = c(beta, gamma, delta),
  ) %>%
  as.data.frame() %>%
  as_tibble() %>%
  rename(susceptible = S,
         prevalence = I,
         #confirmed = I,
         recovered = R,
         dead = D,
         population = population.S)
}

## The observed, cumulative numbers.
observed <- covid19.regional() %>%
  select(susceptible, confirmed, recovered, dead, prevalence, deaths, recoveries, deaths)

observed

expected <- select(do.call(sird, first(observed)),
                   !c(time, population))

expected

## FIXME: `optimx` hates this, and refuses to run when the square of the
## differences is used.
objfun <- function(par) {
  expected <- do.call(sird, as.list(par))
  ##sum((observed$prevalence - expected$prevalence)^2, na.rm = TRUE)
  sum(observed^2, -(expected^2), na.rm = TRUE)
}

## Using educated guesses at parameter values for the deterministic simulation,
## obtain an estimate for each parameter using optimx.
parameterEstimates <-
  optimx(
    par = c(
      beta = 0.09,
      gamma = 0.02,
      delta = 0.001
    ),
    fn = objfun,
    method = "L-BFGS-B",
    lower = c(0, 0, 0)
  )

parameterEstimates

fitted <-
  with(first(observed),
       sird(susceptible = susceptible,
            prevalence = prevalence,
            confirmed = confirmed,
            recovered = recovered,
            dead = dead,
            deaths = deaths,
            recoveries = recoveries,
            beta = parameterEstimates$beta,
            gamma = parameterEstimates$gamma,
            delta = parameterEstimates$delta)) #%>%
  ## FIXME: I have ABSOLUTLEY no idea why confirmed isn't the cumulative sum
  ## here, because the value returned by `sird` should be the cumulative sum! It
  ## is for expected! That's what I programmed! What?!
  #mutate(confirmed = cumsum(confirmed))

fitted

# Ashok's plotting code ---------------------------------------------------
stopifnot(nrow(observed) == nrow(fitted))

times <- seq_along(observed$dead)

plotData <-
  tibble(
    date = times,
    observed_I = observed$confirmed,
    observed_R = observed$recovered,
    observed_D = observed$dead,
    fitted_I = fitted$prevalence, #fitted$confirmed,
    fitted_R = fitted$recovered,
    fitted_D = fitted$dead
  )
  
plot(
  times,
  fitted$confirmed,
  col = "blue",
  type = "l",
  lwd = 2,
  bty = "l",
  xlab = "Time",
  ylab = "Compartment Size",
  main = "IRD Epidemic Compartments",
  ## MAYBE FIXME: if the sum of suceptible, confirmed, recovered, and dead is
  ## used then the population is inflated... why?
  ylim = c(0, summarize(first(covid19.regional()),
                        population = sum(susceptible,
                                         infections,
                                         recoveries,
                                         deaths))$population)
)
lines(times, fitted$recovered, lwd = 2, col = "darkgreen")
lines(times, fitted$dead, lwd = 2, col = "red")
abline(v = 0, h = 0)

# Plot observed vs fitted values
p1 <- ggplot(plotData, aes(x = date)) +
  geom_line(aes(y = observed_I, color = "Observed Prevalence")) +
  geom_line(aes(y = fitted_I, color = "Fitted Prevalence")) +
  labs(y = "Prevalence", color = "Legend") +
  theme_minimal()

p1 

p2 <- ggplot(plotData, aes(x = date)) +
  geom_line(aes(y = observed_R, color = "Observed Cumulative Recovered")) +
  geom_line(aes(y = fitted_R, color = "Fitted Recovered")) +
  labs(y = "Recovered", color = "Legend") +
  theme_minimal()

p2

p3 <- ggplot(plotData, aes(x = date)) +
  geom_line(aes(y = observed_D, color = "Observed Cumulative Deaths")) +
  geom_line(aes(y = fitted_D, color = "Fitted Deaths")) +
  labs(y = "Deaths", color = "Legend") +
  theme_minimal()

p3

grid.arrange(p1, p2, p3, ncol = 1)