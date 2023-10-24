library(sf)
library(rstudioapi)
library(dplyr)
library(tmap)
library(terra)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Read data
charging_ports = read.csv("national-charge-point-registry_modified.csv")
lsoa_boundaries = st_read("LSOA_(Dec_2011)_Boundaries_Generalised_Clipped_(BGC)_EW_V3.geojson")
income = read.csv("Gross_Individual_Income_LSOA.csv")

# Processing the charging port data by removing empty location and convert into sf
missing_rows = which(is.na(charging_ports$longitude) | is.na(charging_ports$latitude))
charging_ports = charging_ports[-missing_rows, ]
charging_ports_sf = st_as_sf(charging_ports, coords = c("longitude", "latitude"), crs = 4326)

# Filter out charging port NOT in wales/england
charging_ports_in_lsoa = st_filter(charging_ports_sf, lsoa_boundaries)


# Join income data with LSOA geometry
# Although doesn't matter, inner makes most sense here because missing on either side should be omitted
income_join = inner_join(lsoa_boundaries, income, join_by("LSOA11CD" == "LSOA.code"))

# Check for any invalid geometry before plotting it (takes a long time to plot!)
validity = st_is_valid(income_join)
invalid_geometries = income_join[!validity, ]

tmap_mode("plot")

# Plotting map
map = tm_shape(income_join)+
  tm_fill(col = "Income", alpha = 0.7, palette = "magma", title = "Medium Gross Income",
          style = "quantile")+
  tm_shape(charging_ports_in_lsoa)+
  tm_dots(col = "blue", size = 0.00002)+
  #tm_logo("*.png", height = 2) +
  tm_scale_bar(position = c("right", "bottom"), width = 0.25) +
  tm_compass(position = c("right", "top"), size = 2.5)

tmap_save(map, filename = "EV Charging Port VS Income v0.1.png", width = 1440, height = 1920)
