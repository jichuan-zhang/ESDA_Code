library(tmap)
library(sf)
library(readxl)
library(rstudioapi)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Step 1: Load the Data
power_plant_data <- read_excel("power plant Indonesia.xlsx")

# Step 2: Load the Shapefile
indonesia_country_shape <- st_read("idn_adm_bps_20200401_shp/idn_admbnda_adm0_bps_20200401.shp")
indonesia_province_shape <- st_read("idn_adm_bps_20200401_shp/idn_admbnda_adm1_bps_20200401.shp")

# Step 3: Data Preparation
# Assuming you have latitude and longitude in your power plant data and a way to match them with the shapefile
# You might need to perform spatial join or merge based on administrative areas

# Step 4: Create the Map
tm_shape(indonesia_shape) +
  tm_borders() +
  tm_shape(power_plant_data) +
  tm_symbols(col = "type", # Column representing type of generation
             size = "capacity", # Column representing generating capacity
             shape = "status", # Column representing status
             scale = 2, 
             legend.shape.show = TRUE,
             legend.col.show = TRUE,
             legend.size.show = TRUE) +
  tm_layout(title = "Power Plants in Indonesia")