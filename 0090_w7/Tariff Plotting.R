library(ggplot2)
library(lubridate)
library(rstudioapi)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

Tariffs <- read.csv("Tariffs2013.csv", stringsAsFactors = FALSE)

Tariffs$time <- dmy_hm(Tariffs$TariffDateTime)
Tariffs$Tariff <- factor(Tariffs$Tariff, levels = c("Low", "Normal", "High"))

start_date <- min(floor_date(Tariffs$time, "month"))
end_date <- max(ceiling_date(Tariffs$time, "month"))
months <- seq(start_date, end_date, by = "month")

for (i in 1:(length(months) - 1)) 
  {
  month_start <- months[i]
  month_end <- months[i + 1] - seconds(1)
  
  subset_Tariffs <- Tariffs[Tariffs$time >= month_start & Tariffs$time < month_end, ]
  
  plot <- ggplot(subset_Tariffs, aes(x = time, y = Tariff)) +
    geom_point() +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%d %b") +
    labs(x = "Time", y = "Tariff", title = paste("Tariff Levels for", format(month_start, "%B %Y"))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  file_name <- paste("Tariff_Levels_", format(month_start, "%Y_%m"), ".png", sep = "")

  ggsave(file_name, plot = plot, width = 10, height = 8, units = "in")
}
