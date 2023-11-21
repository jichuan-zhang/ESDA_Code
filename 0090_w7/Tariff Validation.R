library(ggplot2)
library(lubridate)
library(dplyr)
library(rstudioapi)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

Tariffs <- read.csv("Tariffs2013.csv", stringsAsFactors = FALSE)

Tariffs$TariffDateTime <- dmy_hm(Tariffs$TariffDateTime)
Tariffs <- Tariffs %>% rename(time = TariffDateTime)

average_consumption_2013 <- read.csv("average_consumption_2013.csv") %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

average_consumption_2013 <- left_join(average_consumption_2013, Tariffs, join_by("DateTime" == "time"))


real_tariff <- data.frame(Tariff = c("Low", "Normal", "High"), Value = c(0.0399, 0.1176, 0.6720))
real_tariff <- left_join(average_consumption_2013, real_tariff, join_by("Tariff" == "Tariff"))
real_tariff <- real_tariff %>%
  mutate(cost = Average_KWH * Value)
real_tariff_annual_cost <- real_tariff %>%
  group_by(Acorn_grouped) %>%
  summarise(total_cost = sum(cost, na.rm = TRUE), .groups = "drop")

real_tariff_jan <- real_tariff %>%
  filter(DateTime >= as.POSIXct("2013-01-01") & DateTime < as.POSIXct("2013-02-01"))
real_tariff_jan_cost <- real_tariff_jan %>%
  group_by(Acorn_grouped) %>%
  summarise(total_cost = sum(cost, na.rm = TRUE), .groups = "drop")

custom_tariff <- data.frame(Tariff = c("Low", "Normal", "High"), Value = c(0.0399, 0.1176, 0.6720))
custom_tariff <- left_join(average_consumption_2013, custom_tariff, join_by("Tariff" == "Tariff"))


