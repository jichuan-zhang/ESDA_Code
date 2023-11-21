library(data.table)
library(rstudioapi)
library(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- fread("CC_LCL-FullData.csv") 
## https://data.london.gov.uk/dataset/smartmeter-energy-use-data-in-london-households
## dataset way too big to upload
acorn <- fread("informations_households.csv")

# Rename and forcefully convert to numeric
data <- data %>% 
  rename(KWH_per_half_hour = `KWH/hh (per half hour)`) %>%
  mutate(KWH_per_half_hour = as.numeric(as.character(KWH_per_half_hour)))

# Addressing any NAs introduced during the conversion
data <- na.omit(data)

# Convert DateTime to POSIXct format
data[, DateTime := as.POSIXct(DateTime, format = "%d-%m-%Y %H:%M:%S")]

# Remove rows with 'Std' in stdorToU
data <- data[stdorToU != "Std"]

acorn <- acorn[,-c(2,3,5)]

data <- left_join(data, acorn, join_by("LCLid" == "LCLid"))

data <- data[Acorn_grouped != "ACORN-"]
data <- data[Acorn_grouped != "ACORN-U"]

# Filter data for 2013
data_2013 <- data[DateTime >= as.POSIXct("2013-01-01") & DateTime < as.POSIXct("2014-01-01")]

data_2013_df <- as.data.frame(data_2013)

average_consumption_2013 <- data_2013_df %>%
  group_by(Acorn_grouped, DateTime) %>%
  summarize(Average_KWH = mean(KWH_per_half_hour, na.rm = TRUE), .groups = 'drop')

average_consumption_2013 <- average_consumption_2013 %>%
  mutate(DateTime = format(DateTime, "%Y-%m-%d %H:%M:%S"))

write.csv(average_consumption_2013, file = "average_consumption_2013.csv", row.names = FALSE)
