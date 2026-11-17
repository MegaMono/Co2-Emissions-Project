# -----------------------------------------------------------------------------#
# Randall Coon
# COMI-1350-600
# Final Project
# CO2 Emissions
# Due: May 5, 2026
#------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#

# Data Source:
# Our World in Data - CO2 and Greenhouse Gas Emissions
# https://github.com/owid/co2-data

# Research Question:
# How have CO2 emissions changed in the United States, China and Europe from 1950–2023?

# This script imports and manipulates CO2 emission data from Our World in Data.
# It filters the countries and years speculated and calculates the total CO2 emissions,
# percent change within selected year range and calculates per capita emission.
# Visualization is produced to show fitted linear regression models and long term
# trend evaluation.

library(tidyverse)
library(dslabs)
library(dplyr)

# Import data set.
# Read csv file from adjacent file.
co2 <- read.csv("owid-co2-data.csv")

# Show first rows of raw dataset.
head(co2)

# Show all variable names.
names(co2)

# co2_compare: filter United States, China, Europe between 1950 and 2023.
# Omit CO2 and population data with NA values.
# Calculate percent change and CO2 emissions within date range.

co2_compare <- co2 |>
  # Filter rows for country and date range, omitting NA values.
  filter(
    country %in% c("United States", "China", "Europe"), 
    year >= 1950 & year < 2024,
    !is.na(co2),
    !is.na(population)
  ) |>
  
  # Select columns with country, year, CO2 and population.
  select(country, year, co2, population) |>
  
  # Sort data by country and year.
  arrange(country, year) |> 
  
  # Perform the calculations per country.
  group_by(country) |>
  
  # Percent change calculation measures the change in emissions
  # relative to the first year.
  # Per capita shows the total emissions divided by population
  # for clear comparison of country population size.
  mutate(
    percent_change = ((co2 - co2[1]) / co2[1]) * 100,
    co2_per_capita = co2 / population
  )

co2_compare

# Summary table shows overall comparison of percent change
# throughout the date range between selected countries.
summary_table <- co2_compare |>
  group_by(country) |>
  summarize(
    
    # Emissions first year.
    start = co2[1],
    
    # Emissions final year.
    end = co2[length(co2)],
    
    # Overall percent change.
    percent_change = ((end - start) / start) * 100
  )

summary_table

#------------------------------------------
# Plot 1: Total CO2 emissions over selected time range.
#------------------------------------------

# CO2 emission by region for United States, Europe and China.
ggplot(
  co2_compare, 
       aes(year, co2, color = country))+
  
  # Lines show total emission per country.
  geom_line(size = .7) +
  
  # Labels
  labs(
    title = "CO2 Emissions by Region (1950-2023)",
    x = "Year",
    y = "CO2 Emissions"
  ) +

  theme_minimal() 
  
#------------------------------------------
# Plot 2: Emissions per capita
#------------------------------------------

# Compare emissions based on population size.
ggplot(co2_compare, aes(year, co2_per_capita, color = country)) +
  
  # Lines represent emission fluctuation per capita over time.
  geom_line(size = .7) +
  
  # Labels
  labs(
    title = "CO2 Emissions Per Capita (1950–2023)",
    x = "Year",
    y = "CO2 per capita"
  ) +
  
  theme_minimal()

#------------------------------------------
# Plot 3: Percent change since 1950
#------------------------------------------

# Relative growth comparison for each country.
ggplot(co2_compare,
       aes(year, percent_change, color = country)) +
  
  # Show growth comparison line.
  geom_line(size = .7) +
  
  # Show labels.
  labs(
    title = "Percent Change in CO2 Emissions (1950-2023)",
    x = "Year",
    y = "Percent Change"
  
    ) +
  
  theme_minimal() 

#------------------------------------------
# Plot 4: Linear regression plot.
#------------------------------------------

# Fit model linear regression with '*' interaction terms.
co2_model <- lm(co2 ~ year * country, data = co2_compare)

# Diagnostic plots
summary(co2_model)
par(mfrow=c(2,2))
plot(co2_model)

# Add fitted values back into data set.
co2_compare$predicted <- predict(co2_model)

# Plot data points and regression line.
ggplot(co2_compare,
       aes(year, co2, color = country)) +
  
  # Show data points
  geom_point() +
  
  # Lines representing average emission trends.
  geom_line(size = .7) +
  
  # Show calculated linear regression line.
  geom_line(aes(y = predicted), linetype = "dashed") +

  # Show labels.
  labs(
    title = "CO2 Emissions with Linear Regression (1950-2023)",
    x = "Year",
    y = "CO2 Emissions",
  ) +
    
  theme_minimal() 

