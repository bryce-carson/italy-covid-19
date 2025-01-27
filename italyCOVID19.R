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

## This is included just in case you don't run the files in their sorted order.
source("0.ashok_ggplot_theme.R")
source("0.COVID-19_in_Italy_functions.R")

## ▂▃▅▇█▓▒░۩۞۩ DOWNLOAD COVID-19 EPIDEMIC DATA ۩۞۩░▒▓█▇▅▃▂
## These data are for the provinces of Italy we are interested in at present.
## When we want to study other provinces we can add them to this list. Lombardia
## is the default province and Italy is the default country for this customized
## use of COVID19::covid19() because it was the province studied at the time my
## supervisor asked me to work on this project with him.
observationsItaly <- suppressMessages(list(
  Lombardia = covid19.regional(),
  Campania = covid19.regional(subregion = "Campania"),
  Lazio = covid19.regional(subregion = "Lazio")
))

## Select a province under study by changing the component part following the
## extract operator.
country <- "Italy" # This line only required and used because later plotting code uses it.
subregion <- "Lombardia" # This line only required and used because later plotting code uses it.
observed <- observationsItaly[[subregion]] # Access the list component directly, rather than slice the list with `[`.

## ▂▃▅▇█▓▒░۩۞۩ INITIAL STATE VARIABLES ۩۞۩░▒▓█▇▅▃▂
## A perfectly acceptable, "base R" way of accessing these values.
init <- c(S = observed$susceptible[1],
          I = observed$prevalence[1],
          R = observed$recovered[1],
          D = observed$dead[1])

## An alternative, "tidyverse" way of doing the same thing. Pick whichever is
## more beautiful in your eyes. This variable is unused.
initialStateVariables <-
  first(observed) %>%
  select(susceptible, prevalence, recovered, dead) %>%
  as.numeric() %>%
  `names<-`(c("S", "I", "R", "D"))

## ▂▃▅▇█▓▒░۩۞۩ OPTIMIZATION ۩۞۩░▒▓█▇▅▃▂
## Initial guesses for parameters
initial_guesses <- list(
  c(beta = 0.4, gamma = 0.05, delta = 0.01),
  c(beta = 0.3, gamma = 0.05, delta = 0.02),
  c(beta = 0.5, gamma = 0.05, delta = 0.03)
)

## Tighter and more realistic bounds
lower_bounds <- c(beta = 0.001, gamma = 0.001, delta = 0.0001)
upper_bounds <- c(beta = 1, gamma = 0.5, delta = 0.1)

## Computed serially and without vectorization
for (guess in initial_guesses) {
  opt_params <- fit_optimization(guess, lower_bounds, upper_bounds)
  if (!is.null(opt_params)) break
}

if (is.null(opt_params)) {
  stop("Optimization did not converge with any initial guesses.")
}

## ▂▃▅▇█▓▒░۩۞۩ BASIC REPRODUCTION NUMBER ۩۞۩░▒▓█▇▅▃▂
## The Basic Reproduction Number (R0) for an SIRD model is calculated as
## follows: R0 = beta/(gamma + delta)

## The following statement is effective, but doesn't communicate what it is
## really doing to the reader (except with R0). In six months, it'll take you a
## couple minutes to piece together that opt_params is arranged in the order:
## beta, gamma, delta. A question to ask is why should we remove the names with
## `[[` when they can help us instead? See the printed results of opt_params!
R0 <- opt_params[[1]] / (opt_params[[2]] + opt_params[[3]])

## A very short, very clear statement.
R0 <- with(as.list(opt_params), beta / (gamma + delta))

## ▂▃▅▇█▓▒░۩۞۩ BASIC REPRODUCTION NUMBER ۩۞۩░▒▓█▇▅▃▂
sird1YearResults <- as.data.frame(lsoda(y = init, times = 1:365, func = SIRD, parms = opt_params))
sird1YearResults$date <- as.Date(first(observed$date) : (first(observed$date) + 364))

## ▂▃▅▇█▓▒░۩۞۩ PLOTTING RESULTS ۩۞۩░▒▓█▇▅▃▂
## ▂▃▅▇█▓▒░۩۞۩ OBSERVED PLOTS ۩۞۩░▒▓█▇▅▃▂
## This is just one way in which work can be organized: using lists. The
## identifier `observedPlots` documents what data these plots are derived from,
## and most text editors will allow you to collapse the list so that you can
## ignore it once you're done with it.
observedPlots <- list(
  ## Plot daily cases (Incidence)
  incidence = ggplot(observed, aes(x = date, y = newCases)) +
              geom_line(color = "blue") +
              labs(title = paste0("Daily Cases in %s, %s", subregion, country),
                   x = "Date",
                   y = "Daily Cases") +
              scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
              ashokTheme,

  ## Plot Prevalence (Active cases)
  prevalence = ggplot(observed, aes(x = date, y = prevalence)) +
    geom_line(color = "black") +
    labs(title = paste0("Prevalence in %s, %s", subregion, country),
         x = "Date",
         y = "Prevalence") +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
    ashokTheme,

  ## Plot daily recovered
  newRecovered = ggplot(observed, aes(x = date, y = newRecovered)) +
    geom_line(color = "darkgreen") +
    labs(title = paste0("Daily Recovered in %s, %s", subregion, country),
         x = "Date",
         y = "Daily Recovered") +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
    ashokTheme,

  ## Plot daily deaths
  newDead = ggplot(observed, aes(x = date, y = newDead)) +
    geom_line(color = "red") +
    labs(title = paste0("Daily Deaths in %s, %s", subregion, country),
         x = "Date",
         y = "Daily Deaths") +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
    ashokTheme
)

## ▂▃▅▇█▓▒░۩۞۩ MODEL RESULT PLOTS ۩۞۩░▒▓█▇▅▃▂
prevalenceAndInfectedPlot <- ggplot() +
  geom_line(data = observed,
            aes(x = date, y = prevalence),
            color = "black",
            linetype = "solid") +
  geom_line(data = sird1YearResults,
            aes(x = date, y = I),
            color = "blue",
            linetype = "dotdash") +
  labs(title = paste0("SIRD Model Fit to COVID-19 Data in %s, %s", subregion, country),
       x = "Date",
       y = "Prevalence") +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%b %d") +
  ashokTheme

recoveredAndRPlot <- ggplot() +
  geom_line(data = observed,
            aes(x = date, y = recovered),
            color = "darkgreen",
            linetype = "solid") +
  geom_line(data = sird1YearResults,
            aes(x = date, y = R),
            color = "blue",
            linetype = "dotdash") +
  labs(title = paste0("SIRD Model Fit to COVID-19 Data in %s, %s", subregion, country),
       x = "Date",
       y = "Recovered") +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%b %d") +
  ashokTheme

deadAndDPlot <- ggplot() +
  geom_line(data = observed,
            aes(x = date, y = dead),
            color = "red",
            linetype = "solid") +
  geom_line(data = sird1YearResults,
            aes(x = date, y = D),
            color = "blue",
            linetype = "dotdash") +
  labs(title = paste0("SIRD Model Fit to COVID-19 Data in %s, %s", subregion, country),
       x = "Date",
       y = "Dead") +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%b %d") +
  ashokTheme

## Set the factor levels for the Compartment variable to ensure the desired order
# outoneYear_long$Compartment <- factor(outoneYear_long$Compartment, levels = c("S", "I", "R", "D"))

## Because "outoneYear_long" isn't the most descriptive name, it makes more
## sense to me to remove the name and simply pipe the dataframe on to ggplot
## directly, since that is its only usage. While "out" is the "output" of an
## earlier function, that's true of every function, so "out" isn't a great
## identifier. Instead it makes more sense to name this for the input
## parameters, the type of model the data contains, etc.
## 
## "_long" to indicate that the data has been transformed by `pivot_longer`
## isn't bad, but it makes more sense to describe the semantics of the
## dataframe's shape, rather than just it's shape (a long versus wide
## rectangular dataframe).
##
## Plot 4: All compartments (SIRD)
allComponentsPlot <- sird1YearResults %>%
  pivot_longer(cols = c(S, I, R, D),
               names_to = "Compartment",
               values_to = "Value") %>%
  ## The effect of the commented code above this pipeline can be achieved with
  ## the following much shorter expression, which takes advantage of the default
  ## functionality of the as.factor function.
  mutate(Compartment = as.factor(Compartment)) %>%
  ggplot(aes(x = time,
             y = Value
        
             ## color = Compartment will colour the compartments with unique values, but
             ## because the colour of these variables is controlled manually with
             ## `scale_color_manual`, there's no use including this argument.
             #color = Compartment
             )
        ) +
    geom_line() +
    scale_color_manual(values = c("S" = "blue",
                                  "I" = "black",
                                  "R" = "darkgreen",
                                  "D" = "red"),
                       labels = c("S" = "Susceptible",
                                  "I" = "Infected",
                                  "R" = "Recovered",
                                  "D" = "Dead")) +
    labs(title = paste0("SIRD Model Fit to COVID-19 Data in %s, %s", subregion, ", ", country), 
         x = "Time (days)",
         y = "Compartment Size",
         color = "Compartment") +
    ashokTheme

## ▂▃▅▇█▓▒░۩۞۩ Plots of Point Prevalence and Incidence Proportion ۩۞۩░▒▓█▇▅▃▂
epidemicStatisticPlots <-
  with(
    list(
      plotData = 
        mutate(sird1YearResults,
              N = sum(S, I, R),
              pointPrevalence = I / N,
              pointPrevalenceRate = pointPrevalence - lag(pointPrevalence),
              incidenceProportion = (I - lag(I)) / N)
    ),
    list(
      pointPrevalence = ggplot(plotData) +
        geom_line(aes(x = date, y = pointPrevalence)) + 
        ashokTheme,
      pointPrevalenceRate = ggplot(plotData) +
        geom_line(aes(x = date, y = pointPrevalenceRate)) +
        ashokTheme,
      incidenceProportion = ggplot(plotData) +
        geom_line(aes(x = date, y = incidenceProportion)) +
        ashokTheme
    )
  )
