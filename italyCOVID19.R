library(tidyverse)
library(magrittr)
library(COVID19)
library(deSolve)
library(optimx)
library(countrycode)
library(gridExtra)
library(ggplot2)

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

country <- "Italy"
subregion <- "Lombardia" # "Campania" #  "Lazio" # 
startDate <- as.Date("2020-08-31")
endDate <- as.Date("2020-11-30")

covid19.regional <- function(country, subregion, startDate, endDate) {
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

#print(prettyNum(regionalPopulation, big.mark = ","))

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
    slice(-1)
}

observed <- covid19.regional(country = country,
                             subregion = subregion,
                             startDate = startDate,
                             endDate = endDate) %>%
  select(date, susceptible, confirmed, recovered, dead, prevalence, newCases, newDead, newRecovered)

observed

#print(observed, n = dim(observed)[1])

# Plot daily cases (Incidence)
plot_Incidence <- ggplot(observed, aes(x = date, y = newCases)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_hline(yintercept = min(observed$newCases), color = "black") +
  geom_vline(xintercept = min(observed$date), color = "black") +
  labs(title = paste0("Daily Cases in ", subregion, ", ", country), x = "Date", y = "Daily Cases") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_Incidence

# Plot Prevalence (Active cases)
plot_Prevalence <- ggplot(observed, aes(x = date, y = prevalence)) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_hline(yintercept = min(observed$prevalence), color = "black") +
  geom_vline(xintercept = min(observed$date), color = "black") +
  labs(title = paste0("Prevalence in ", subregion, ", ", country), x = "Date", y = "Prevalence") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_Prevalence

# Plot daily recovered
plot_newRecovered <- ggplot(observed, aes(x = date, y = newRecovered)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_hline(yintercept = min(observed$newRecovered), color = "black") +
  geom_vline(xintercept = min(observed$date), color = "black") +
  labs(title = paste0("Daily Recovered in ", subregion, ", ", country), x = "Date", y = "Daily Recovered") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_newRecovered

# Plot daily deaths
plot_newDead <- ggplot(observed, aes(x = date, y = newDead)) +
  geom_line(color = "red", linewidth = 1.2) +
  geom_hline(yintercept = min(observed$newDead), color = "black") +
  geom_vline(xintercept = min(observed$date), color = "black") +
  labs(title = paste0("Daily Deaths in ", subregion, ", ", country), x = "Date", y = "Daily Deaths") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_newDead

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

# Time vector
times <- seq(1, nrow(observed), by = 1)

# Initial state values
init <- c(S = first(observed$susceptible),
          I = first(observed$prevalence),
          R = first(observed$recovered),
          D = first(observed$dead))

# Objective function to minimize the residual sum of squares (RSS)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma", "delta")
  out <- lsoda(y = init, times = times, func = SIRD, parms = parameters)
  sum(#(observed$susceptible - out[, "S"])^2 +
        (observed$prevalence - out[, "I"])^2 +
        (observed$recovered - out[, "R"])^2 +
        (observed$dead - out[, "D"])^2)
}

# Initial guesses for parameters
initial_guesses <- list(
  c(beta = 0.4, gamma = 0.05, delta = 0.01),
  c(beta = 0.3, gamma = 0.05, delta = 0.02),
  c(beta = 0.5, gamma = 0.05, delta = 0.03)
)

# Tighter and more realistic bounds
lower_bounds <- c(beta = 0.001, gamma = 0.001, delta = 0.0001)
upper_bounds <- c(beta = 1, gamma = 0.5, delta = 0.1)

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

# beta       gamma      delta 
# 0.06573343 0.02737643 0.00010000

# The Basic Reproduction Number (R0) for an SIRD model
# R0 = beta/(gamma + delta)

R0 <- opt_params[[1]]/(opt_params[[2]]+opt_params[[3]])
R0

# opt_params[[3]] <- 0.001 # I have reset delta to a different value. 
# 
# R0 <- opt_params[[1]]/(opt_params[[2]]+opt_params[[3]])
# R0

# Solve the SIRD model with the optimized parameters
out <- lsoda(y = init, times = times, func = SIRD, parms = opt_params)
out

# Convert the output to a data frame for plotting
output <- as.data.frame(out)
output$date <- observed$date

# Plot the results

# Plot 1: Prevalence and I (Infected)
ggplot() +
  geom_line(data = observed, aes(x = date, y = prevalence), color = "black", linetype = "solid", linewidth = 1.2) +
  geom_line(data = output, aes(x = date, y = I), color = "blue", linetype = "dotdash", linewidth = 1.2) +
  geom_hline(yintercept = min(observed$prevalence), color = "black") +
  geom_vline(xintercept = min(observed$date), color = "black") +
  labs(title = paste0("SIRD Model Fit to COVID-19 Data in ", subregion, ", ", country), x = "Date", y = "Prevalence") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Plot 2: Recovered and R (Recovered)
ggplot() +
  geom_line(data = observed, aes(x = date, y = recovered), color = "darkgreen", linetype = "solid", linewidth = 1.2) +
  geom_line(data = output, aes(x = date, y = R), color = "blue", linetype = "dotdash", linewidth = 1.2) +
  geom_hline(yintercept = min(observed$recovered), color = "black") +
  geom_vline(xintercept = min(observed$date), color = "black") +
  labs(title = paste0("SIRD Model Fit to COVID-19 Data in ", subregion, ", ", country), x = "Date", y = "Recovered") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Plot 3: Dead and D (Dead)
ggplot() +
  geom_line(data = observed, aes(x = date, y = dead), color = "red", linetype = "solid", linewidth = 1.2) +
  geom_line(data = output, aes(x = date, y = D), color = "blue", linetype = "dotdash", linewidth = 1.2) +
  geom_hline(yintercept = min(observed$dead), color = "black") +
  geom_vline(xintercept = min(observed$date), color = "black") +
  labs(title = paste0("SIRD Model Fit to COVID-19 Data in ", subregion, ", ", country), x = "Date", y = "Dead") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Run the SIRD model with the optimized parameters for 1 year
oneYear <- seq(1, 365, by = 1)

outoneYear <- lsoda(y = init, times = oneYear, func = SIRD, parms = opt_params)
outoneYear <- as.data.frame(outoneYear)

outoneYear_long <- outoneYear %>%
  pivot_longer(cols = c(S, I, R, D), names_to = "Compartment", values_to = "Value")

# Set the factor levels for the Compartment variable to ensure the desired order
outoneYear_long$Compartment <- factor(outoneYear_long$Compartment, levels = c("S", "I", "R", "D"))

ggplot(outoneYear_long, aes(x = time, y = Value, color = Compartment)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = min(outoneYear$I), color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  scale_color_manual(values = c("S" = "blue", "I" = "black", "R" = "darkgreen", "D" = "red"),
                     labels = c("S" = "Susceptible", "I" = "Infected", "R" = "Recovered", "D" = "Dead")) +
  labs(title = paste0("SIRD Model Fit to COVID-19 Data in ", subregion, ", ", country), 
       x = "Time (days)", y = "Compartment Size", color = "Compartment") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# observed_long <- observed %>% 
#   select(-susceptible, -confirmed, -deaths, -newRecovered) %>% 
#   pivot_longer(cols = -date, names_to = "compartment", values_to = "observed")
# 
# output_long <- output %>%
#   select(-S, -time) %>%
#   pivot_longer(cols = -date, names_to = "compartment", values_to = "predicted")
# 
# plot_data <- full_join(observed_long, output_long, by = c("date", "compartment"))
# 
# ggplot(plot_data, aes(x = date)) +
#   geom_line(aes(y = observed, color = compartment), linewidth = 1.2) +
#   geom_line(aes(y = predicted, color = compartment), linetype = "dashed", linewidth = 1.2) +
#   labs(title = "SIRD Model Fit to COVID-19 Data",
#        x = "Date",
#        y = "Count",
#        color = "Compartment") +
#   theme_minimal()
