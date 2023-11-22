library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)

# Set working directory to the location of your R script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load data
Tariffs <- read_csv("Tariffs2013.csv")
average_consumption_2013 <- read_csv("average_consumption_2013.csv")

# Convert date-times and merge datasets
Tariffs <- Tariffs %>%
  mutate(time = dmy_hm(TariffDateTime)) %>%
  select(-TariffDateTime)

average_consumption_2013 <- average_consumption_2013 %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
  inner_join(Tariffs, by = c("DateTime" = "time"))

# Define real and custom tariffs
real_tariff <- data.frame(Tariff = c("Low", "Normal", "High"), Value = c(0.0399, 0.1176, 0.6720))
custom_tariff <- data.frame(Tariff = c("Low", "Normal", "High"), Value = c(0.04, 0.1, 0.958))

consumption_annual <- average_consumption_2013 %>%
  group_by(Acorn_grouped) %>%
  summarise(total_consumption = sum(Average_KWH, na.rm = TRUE), .groups = "drop")

consumption_tariff_level_annual <- average_consumption_2013 %>%
  group_by(Acorn_grouped, Tariff) %>%
  summarise(consumption = sum(Average_KWH, na.rm = TRUE), .groups = "drop") %>%
  left_join(consumption_annual, by = "Acorn_grouped") %>%
  mutate(percentage = consumption / total_consumption) %>%
  left_join(real_tariff, by = "Tariff") %>%
  left_join(custom_tariff, by = "Tariff") %>%
  rename(real_tariff = Value.x, custom_tariff = Value.y)


# Apply real tariff
average_consumption_2013 <- average_consumption_2013 %>%
  left_join(real_tariff, by = "Tariff") %>%
  mutate(cost = Average_KWH * Value)

# Apply custom tariff
average_consumption_2013 <- average_consumption_2013 %>%
  left_join(custom_tariff, by = "Tariff", suffix = c("", "_custom")) %>%
  mutate(custom_cost = Average_KWH * Value_custom)

# Calculate monthly costs for real and custom tariffs
monthly_costs_comparison <- average_consumption_2013 %>%
  group_by(Acorn_grouped, Month = floor_date(DateTime, "month")) %>%
  summarise(cost = sum(cost, na.rm = TRUE),
            custom_cost = sum(custom_cost, na.rm = TRUE),
            .groups = "drop")

# Reshape data for line plot
long_format <- monthly_costs_comparison %>%
  pivot_longer(cols = c(cost, custom_cost), names_to = "TariffType", values_to = "Cost")

# Line plot
l <-ggplot(long_format, aes(x = Month, y = Cost, color = Acorn_grouped, linetype = TariffType)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Monthly Cost Comparison across Acorn Groups",
       x = "Month",
       y = "Cost") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Make sure the Month column is a factor and in the correct order
monthly_costs_comparison$Month <- factor(monthly_costs_comparison$Month,
                                         levels = sort(unique(monthly_costs_comparison$Month)))

# Reshape data for bar plot
long_format_bar <- monthly_costs_comparison %>%
  gather(key = "TariffType", value = "Cost", -Acorn_grouped, -Month)

# We create a list to store each of our plots
plots_list <- list()

# Separate plots for each month
months <- unique(monthly_costs_comparison$Month)
for (m in months) {
  # Filter data for the month
  monthly_data <- long_format_bar %>%
    filter(Month == m)
  
  # Create the plot for the month
  p <- ggplot(monthly_data, aes(x = Acorn_grouped, y = Cost, fill = TariffType)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
    scale_fill_manual(values = c("blue", "red")) +
    labs(title = paste("Cost Comparison for", m),
         x = "Acorn Group",
         y = "Cost") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  
  # Add the plot to the list
  plots_list[[m]] <- p
}

# Calculate the annual totals and create a plot
annual_data <- monthly_costs_comparison %>%
  group_by(Acorn_grouped) %>%
  summarise(cost = sum(cost, na.rm = TRUE),
            custom_cost = sum(custom_cost, na.rm = TRUE)) %>%
  pivot_longer(cols = c(cost, custom_cost), names_to = "TariffType", values_to = "Cost")

# Create the annual total plot
annual_plot <- ggplot(annual_data, aes(x = Acorn_grouped, y = Cost, fill = TariffType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Annual Cost Comparison",
       x = "Acorn Group",
       y = "Cost") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Add the annual plot to the list
plots_list[["Annual"]] <- annual_plot

#print(l)
print(plots_list[["Annual"]])


ggsave("line_plot.png", plot = l, width = 8, height = 6, dpi = 300)
for (name in names(plots_list)) {
  file_name <- paste0(name, "_cost_comparison.png") # creates the full file path
  ggsave(file_name, plot = plots_list[[name]], width = 8, height = 6, dpi = 300)
}

write_csv(annual_data, "annual_cost.csv")
write_csv(consumption_tariff_level_annual, "consumption_tariff_level_annual.csv")