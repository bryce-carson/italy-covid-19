## These data are for the provinces of Italy we are interested in at present.
## When we want to study other provinces we can add them to this list. Lombardia
## is the default province and Italy is the default country for this customized
## use of COVID19::covid19() because it was the province studied at the time my
## supervisor asked me to work on this project with him.
observationsItaly <- suppressMessages(list(
  Lombardia = covid19.regional(),
  Campania = covid19.regional(subregion = "Campania"),
  Lazio = covid19.regional(subregion = "Lazio"),
))

## Select a province under study by changing the component part following the
## extract operator.
observed <- observationsItaly$Lombardia
 
# Plot daily cases (Incidence)
plot_Incidence <- ggplot(observed, aes(x = date, y = newCases)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(title = paste0("Daily Cases in ", subregion, ", ", country), x = "Date", y = "Daily Cases") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  ashokTheme

plot_Incidence

# Plot Prevalence (Active cases)
plot_Prevalence <- ggplot(observed, aes(x = date, y = prevalence)) +
  geom_line(color = "black", linewidth = 1.2) +
  labs(title = paste0("Prevalence in ", subregion, ", ", country), x = "Date", y = "Prevalence") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  ashokTheme

plot_Prevalence

# Plot daily recovered
plot_newRecovered <- ggplot(observed, aes(x = date, y = newRecovered)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  labs(title = paste0("Daily Recovered in ", subregion, ", ", country), x = "Date", y = "Daily Recovered") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  ashokTheme

plot_newRecovered

# Plot daily deaths
plot_newDead <- ggplot(observed, aes(x = date, y = newDead)) +
  geom_line(color = "red", linewidth = 1.2) +
  labs(title = paste0("Daily Deaths in ", subregion, ", ", country), x = "Date", y = "Daily Deaths") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  ashokTheme

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

## A perfectly acceptable, "base R" way of accessing these.
init <- c(S = observed$susceptible[1],
          I = observed$prevalence[1],
          R = observed$recovered[1],
          D = observed$dead[1])

## An alternative, "tidyverse" way of doing the same thing. Pick whichever is
## more beautiful in your eyes.
initialStateVariables <-
  first(observed) %>%
  select(susceptible, prevalence, recovered, dead) %>%
  as.numeric() %>%
  `names<-`(c("S", "I", "R", "D"))

# Initial guesses for parameters
initial_guesses <- list(
  c(beta = 0.4, gamma = 0.05, delta = 0.01),
  c(beta = 0.3, gamma = 0.05, delta = 0.02),
  c(beta = 0.5, gamma = 0.05, delta = 0.03)
)

# Tighter and more realistic bounds
lower_bounds <- c(beta = 0.001, gamma = 0.001, delta = 0.0001)
upper_bounds <- c(beta = 1, gamma = 0.5, delta = 0.1)

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

## Effective, but doesn't communicate what it is really doing to the reader. In
## six months, it'll take you six minutes to realize that opt_params has names
## you can use!
R0 <- opt_params[[1]] / (opt_params[[2]] + opt_params[[3]]) # Why remove the names with `[[` when they can help us instead?

## Effective, but there's no need it should be three lines when base::with
## exists to make the names of an object available during evaluation of an
## expression, without polluting the outer/calling environment.
attach(opt_params)
R0 <- beta / (gamma + delta)
detach(opt_params)

## A very short, very clear statement.
R0 <- with(opt_params, beta / (gamma + delta))

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
  labs(title = paste0("SIRD Model Fit to COVID-19 Data in ", subregion, ", ", country), x = "Date", y = "Prevalence") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  ashokTheme

# Plot 2: Recovered and R (Recovered)
ggplot() +
  geom_line(data = observed, aes(x = date, y = recovered), color = "darkgreen", linetype = "solid", linewidth = 1.2) +
  geom_line(data = output, aes(x = date, y = R), color = "blue", linetype = "dotdash", linewidth = 1.2) +
  labs(title = paste0("SIRD Model Fit to COVID-19 Data in ", subregion, ", ", country), x = "Date", y = "Recovered") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  ashokTheme

# Plot 3: Dead and D (Dead)
ggplot() +
  geom_line(data = observed, aes(x = date, y = dead), color = "red", linetype = "solid", linewidth = 1.2) +
  geom_line(data = output, aes(x = date, y = D), color = "blue", linetype = "dotdash", linewidth = 1.2) +
  labs(title = paste0("SIRD Model Fit to COVID-19 Data in ", subregion, ", ", country), x = "Date", y = "Dead") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  ashokTheme

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
  scale_color_manual(values = c("S" = "blue", "I" = "black", "R" = "darkgreen", "D" = "red"),
                     labels = c("S" = "Susceptible", "I" = "Infected", "R" = "Recovered", "D" = "Dead")) +
  labs(title = paste0("SIRD Model Fit to COVID-19 Data in ", subregion, ", ", country), 
       x = "Time (days)", y = "Compartment Size", color = "Compartment") +
  ashokTheme