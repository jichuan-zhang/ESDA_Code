# Load necessary libraries
library(raster)
library(sf)
library(gstat)
library(sp)

# Your existing code to set the working directory and load the data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
indonesia_shape <- st_read("idn_adm_bps_20200401_shp/idn_admbnda_adm0_bps_20200401.shp")
solar_radiance_brick <- brick("Solar Radiance.nc")
solar_radiance_avg <- calc(solar_radiance_brick, mean, na.rm = TRUE)



