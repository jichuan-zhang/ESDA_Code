# Load necessary packages
library(ggplot2)
library(rstudioapi)
library(dplyr)
library(sf)

# Setting working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the data
data <- read.csv("Indonesia Electricity per Capita.csv")
# Rename columns
names(data) <- c("year", "consumption")

# Fit a linear model
linear_model <- lm(consumption ~ year, data = data)

# Fit a second-order polynomial model
poly2_model <- lm(consumption ~ poly(year, 2, raw=TRUE), data = data)

# Fit a third-order polynomial model
poly3_model <- lm(consumption ~ poly(year, 3, raw=TRUE), data = data)

# Assuming 'data' is your original historical data frame with 'year' and 'consumption' columns
# First, we add three new columns to 'data' that duplicate the 'consumption' values
data$linear_pred <- data$consumption
data$poly2_pred <- data$consumption
data$poly3_pred <- data$consumption

# We then remove the 'consumption' column as instructed
data <- data %>% select(-consumption)

# Prepare the predictions for years 2020 to 2030
years_to_predict <- data.frame(year = 2020:2030)
years_to_predict$linear_pred <- predict(linear_model, newdata = years_to_predict)
years_to_predict$poly2_pred <- predict(poly2_model, newdata = years_to_predict)
years_to_predict$poly3_pred <- predict(poly3_model, newdata = years_to_predict)

# Combine the historical data with the predictions
# We rename the prediction columns in years_to_predict to match those in data
years_to_predict <- rename(years_to_predict, linear_pred = linear_pred, poly2_pred = poly2_pred, poly3_pred = poly3_pred)

# Combine the data frames
combined_data <- rbind(data, years_to_predict)

# Now we plot the combined data
ggplot(combined_data, aes(x = year)) +
  geom_line(aes(y = linear_pred), color = "red") +
  geom_line(aes(y = poly2_pred), color = "green") +
  geom_line(aes(y = poly3_pred), color = "blue") +
  labs(title = "Projected Electricity Consumption Per Capita",
       x = "Year",
       y = "Projected Consumption Per Capita") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("red", "green", "blue"))

summary(linear_model)
summary(poly2_model)
summary(poly3_model)


power_plant_data <- read.csv("power plant Indonesia.csv")

# Constants
population <- 296e6  # Population in 2030
per_capita_consumption_kWh <- 1628  # Per capita consumption in kWh

# Total annual consumption in kWh
total_annual_consumption_kWh <- population * per_capita_consumption_kWh

# Capacity factors
capacity_factors <- c(Wind = 0.346, Solar = 0.246, Hydro = 0.371, Geothermal = 0.71)

# Assuming 'power_plant_data' is loaded and has 'type' and 'capacity_mw' columns
# Match the plant 'type' with its capacity factor and calculate annual generation in kWh
power_plant_data$annual_generation_kWh <- with(power_plant_data, 
                                               8760 * capacity_mw * 1000 * capacity_factors[type])

# Calculate the total annual generation from renewables in kWh
total_renewable_generation_kWh <- sum(power_plant_data$annual_generation_kWh, na.rm = TRUE)

# Required renewable generation is 38% of the total annual consumption
required_renewable_generation_kWh <- total_annual_consumption_kWh * 0.38

# The shortfall in kWh
shortfall_kWh <- required_renewable_generation_kWh - total_renewable_generation_kWh

# Print the shortfall in kWh
print(paste("The shortfall that needs to be made up by solar is:", shortfall_kWh, "kWh"))

# Calculate the shortfall in terms of capacity (MW), assuming continuous generation over the year
shortfall_MW <- shortfall_kWh / (8760 * 1000 * capacity_factors['Solar'])

# Print the shortfall in MW
print(paste("The shortfall in capacity that needs to be made up by solar is:", shortfall_MW, "MW"))
