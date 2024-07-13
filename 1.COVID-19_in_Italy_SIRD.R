# Define the deterministic SIRD model function
sird_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R
    dS <- -beta*S*I / N
    dI <- beta*S*I / N - gamma*I - delta*I
    dR <- gamma*I
    dD <- delta*I
    
    list(c(dS, dI, dR, dD))
  })
}

times <- seq(0, nrow(data) - 1)

# Define the function to solve the SIRD model
solve_sird <- function(parameters) {
  init <- c(S = Susceptible[1], I = Prevalence[1], R = cumRecovered[1], D = cumDeaths[1])
  times <- seq(0, nrow(data) - 1)
  out <- lsoda(y = init, times = times, func = sird_model, parms = parameters)
  as.data.frame(out)
}

# Define the objective function to be minimized
objective_function <- function(parameters) {
  names(parameters) <- c("beta", "gamma", "delta")
  out <- solve_sird(parameters)
  observed <- data[, c("Susceptible", "Prevalence", "cumRecovered", "cumDeaths")]
  fitted <- out[, c("S", "I", "R", "D")]
  sum((observed - fitted)^2, na.rm = TRUE)
}

# Initial parameter guesses
init_params <- c(beta = 0.05, gamma = 0.02, delta = 0.001)

# Fit the model
fit <- optimx(par = init_params, fn = objective_function, method = "L-BFGS-B", lower = c(0, 0, 0))
fit

# Extract fitted parameters
fitted_params <- c(beta = fit$beta, gamma = fit$gamma, delta = fit$delta)
fitted_params

# Solve the SIRD model with fitted parameters
fitted_sird <- solve_sird(fitted_params)
fitted_sird

# Prepare data for plotting
fitted_data <- data.frame(
  date = date,
  observed_I = Prevalence,
  fitted_I = fitted_sird$I,
  observed_R = cumRecovered,
  fitted_R = fitted_sird$R,
  observed_D = cumDeaths,
  fitted_D = fitted_sird$D
)

fitted_data

# plot(times,  fitted_sird$I, col = "blue", type = "l", lwd = 2, bty = "l", xlab = "Time", ylab = "Compartment Size", main = "IRD Epidemic Compartments", ylim = c(0,1500000))
# lines(times, fitted_sird$R, lwd = 2, col = "darkgreen")
# lines(times, fitted_sird$D, lwd = 2, col = "red")
# abline(v = 0, h = 0)

# Plot observed vs fitted values
p1 <- ggplot(fitted_data, aes(x = date)) +
  geom_line(aes(y = observed_I, color = "Observed Prevalence")) +
  geom_line(aes(y = fitted_I, color = "Fitted Prevalence")) +
  labs(y = "Prevalence", color = "Legend") +
  theme_minimal()

p1 

p2 <- ggplot(fitted_data, aes(x = date)) +
  geom_line(aes(y = observed_R, color = "Observed Cumulative Recovered")) +
  geom_line(aes(y = fitted_R, color = "Fitted Recovered")) +
  labs(y = "Recovered", color = "Legend") +
  theme_minimal()

p2

p3 <- ggplot(fitted_data, aes(x = date)) +
  geom_line(aes(y = observed_D, color = "Observed Cumulative Deaths")) +
  geom_line(aes(y = fitted_D, color = "Fitted Deaths")) +
  labs(y = "Deaths", color = "Legend") +
  theme_minimal()

p3

grid.arrange(p1, p2, p3, ncol = 1)