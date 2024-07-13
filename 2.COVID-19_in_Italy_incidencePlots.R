# Plot daily cases (Incidence)
plot_Incidence <- ggplot(data, aes(x = date, y = Incidence)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_hline(yintercept = min(data$Incidence), color = "black") +
  geom_vline(xintercept = min(data$date), color = "black") +
  labs(title = paste0("Daily Cases in ", selectedRegion, ", ", selectedCountry), x = "Date", y = "Daily Cases") +
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
plot_Prevalence <- ggplot(data, aes(x = date, y = Prevalence)) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_hline(yintercept = min(data$Prevalence), color = "black") +
  geom_vline(xintercept = min(data$date), color = "black") +
  labs(title = paste0("Prevalence in ", selectedRegion, ", ", selectedCountry), x = "Date", y = "Prevalence") +
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
plot_newRecovered <- ggplot(data, aes(x = date, y = newRecovered)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_hline(yintercept = min(data$newRecovered), color = "black") +
  geom_vline(xintercept = min(data$date), color = "black") +
  labs(title = paste0("Daily Recovered in ", selectedRegion, ", ", selectedCountry), x = "Date", y = "Daily Recovered") +
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
plot_newDeaths <- ggplot(data, aes(x = date, y = newDeaths)) +
  geom_line(color = "red", linewidth = 1.2) +
  geom_hline(yintercept = min(data$newDeaths), color = "black") +
  geom_vline(xintercept = min(data$date), color = "black") +
  labs(title = paste0("Daily Deaths in ", selectedRegion, ", ", selectedCountry), x = "Date", y = "Daily Deaths") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_newDeaths