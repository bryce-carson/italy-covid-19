# install.packages("COVID19")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("deSolve")
# install.packages("optimx")

rm(list = ls())

library(COVID19)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(deSolve)
library(optimx)
library(DEoptim)

#?covid19

# Change country and regions as needed
selectedCountry <- "Italy"

# Get COVID-19 data for selectedCountry, aggregated by all regions
selectedCountry_data_full <- covid19(country = selectedCountry, level = 2)

# In the above, level is an integer argument denoting the granularity level.
# 1: country-level data. 2: state-level data. 3: lower-level data.

dim(selectedCountry_data_full)
colnames(selectedCountry_data_full)

# Get COVID-19 data for the selected country, aggregated by all regions for a specific date range
selectedCountry_data_trimmed <- covid19(country = selectedCountry, level = 2, start = "2020-08-31", end = "2020-11-30") %>%
  dplyr::select(-id, -government_response_index, -stringency_index, 
         -containment_health_index, -economic_support_index, -administrative_area_level_3,
         -iso_alpha_2, -iso_numeric, -iso_currency, -key_local, -key_jhu_csse, -key_nuts)

dim(selectedCountry_data_trimmed)
colnames(selectedCountry_data_trimmed)

head(selectedCountry_data_trimmed)
tail(selectedCountry_data_trimmed)

# Rename columns
selectedCountry_data_trimmed <- selectedCountry_data_trimmed %>%
  rename(region = administrative_area_level_2, GADM = key_gadm, cumCases = confirmed, cumDeaths = deaths, cumRecovered = recovered)

# Check the column names after renaming
colnames(selectedCountry_data_trimmed)

selectedRegion <- "Lombardia" # "Lazio" # "Campania" # 

# Filter data for a specific region, e.g., Lombardia, Campania, Lazio
regional_data <- selectedCountry_data_trimmed %>% filter(region == selectedRegion)

# Ensure the data has rows
if(nrow(regional_data) == 0) {
  stop("No data available for selected region.")
}

# Check the date column type
regional_data$date <- as.Date(regional_data$date)

# Ensure the data is sorted by date
regional_data <- regional_data %>% arrange(date)

dim(regional_data)
colnames(regional_data)
head(regional_data)
tail(regional_data)

regionalPopulation <- regional_data$population[1]

# Prepare the data for the model by adding daily increase columns
data <- regional_data %>%
  mutate(
    Susceptible = regionalPopulation -  cumCases - cumRecovered - cumDeaths,
    Incidence = c(0, diff(cumCases)),
    newDeaths = c(0, diff(cumDeaths)),
    newRecovered = c(0, diff(cumRecovered)),
    Prevalence = cumCases - cumRecovered - cumDeaths
  ) %>%
  # Correct the prevalence column based on "the formula".
  mutate(Prevalence = lag(Prevalence) + Incidence - newRecovered - newDeaths,
         Susceptible = mapply(sum,
                              regionalPopulation,
                              -Prevalence,
                              -cumRecovered,
                              -cumDeaths),
         .keep = "all") %>%
  slice(-1)

# Display the data
data

dim(data)
colnames(data)

data[ , c(36:40)]

attach(data)

# first(as_tibble(data))
# 
# ## The observed, cumulative numbers.
# observed <- data
# 
# observed
# 
# expected <- select(do.call(sird, first(as_tibble(data))),
#                    !c(time, population))
# 
# expected