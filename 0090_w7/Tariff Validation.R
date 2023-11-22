library(ggplot2)
library(lubridate)
library(dplyr)
library(rstudioapi)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

Tariffs <- read.csv("Tariffs2013.csv", stringsAsFactors = FALSE)

Tariffs$TariffDateTime <- dmy_hm(Tariffs$TariffDateTime)
Tariffs <- Tariffs %>% rename(time = TariffDateTime)

average_consumption_2013 <- read.csv("average_consumption_2013.csv")%>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"))

average_consumption_2013 <- inner_join(average_consumption_2013, Tariffs, join_by("DateTime" == "time"))
## inner join here mainly to deal with previous step introduces some issue with British Summer Time Conversion

real_tariff <- data.frame(Tariff = c("Low", "Normal", "High"), Value = c(0.0399, 0.1176, 0.6720))%>%
  right_join(average_consumption_2013, join_by("Tariff" == "Tariff"))%>%
  mutate(cost = Average_KWH * Value)

real_tariff_annual_cost <- real_tariff %>%
  group_by(Acorn_grouped) %>%
  summarise(total_cost = sum(cost, na.rm = TRUE), .groups = "drop")

consumption_tariff_level_annual <- real_tariff %>%
  group_by(Acorn_grouped, Tariff) %>%
  summarise(total_consumption = sum(Average_KWH, na.rm = TRUE), .groups = "drop")

real_tariff_jan <- real_tariff %>%
  filter(DateTime >= as.POSIXct("2013-01-01") & DateTime < as.POSIXct("2013-02-01"))
real_tariff_jan_cost <- real_tariff_jan %>%
  group_by(Acorn_grouped) %>%
  summarise(total_cost = sum(cost, na.rm = TRUE), .groups = "drop")

custom_tariff <- data.frame(Tariff = c("Low", "Normal", "High"), Value = c(0.0399, 0.1176, 0.6720))%>%
  right_join(average_consumption_2013, join_by("Tariff" == "Tariff"))


