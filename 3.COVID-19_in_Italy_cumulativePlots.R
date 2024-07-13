# Create the ggplot objects for cumulative cases, cumulative recovered and cumulative deaths

plot_cumCases <- ggplot(regional_data, aes(x = date, y = cumCases)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_hline(yintercept = min(regional_data$cumCases), color = "black") +
  geom_vline(xintercept = min(regional_data$date), color = "black") +
  labs(title = paste0("Cumulative Cases in ", selectedRegion, ", ", selectedCountry), x = "Date", y = "Cumulative Cases") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_cumCases

plot_cumRecovered <- ggplot(regional_data, aes(x = date, y = cumRecovered)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_hline(yintercept = min(regional_data$cumRecovered), color = "black") +
  geom_vline(xintercept = min(regional_data$date), color = "black") +
  labs(title = paste0("Cumulative Recovered in ", selectedRegion, ", ", selectedCountry), x = "Date", y = "Cumulative Recovered") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_cumRecovered

plot_cumDeaths <- ggplot(regional_data, aes(x = date, y = cumDeaths)) +
  geom_line(color = "red", linewidth = 1.2) +
  geom_hline(yintercept = min(regional_data$cumDeaths), color = "black") +
  geom_vline(xintercept = min(regional_data$date), color = "black") +
  labs(title = paste0("Cumulative Deaths in ", selectedRegion, ", ", selectedCountry), x = "Date", y = "Cumulative Deaths") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_cumDeaths