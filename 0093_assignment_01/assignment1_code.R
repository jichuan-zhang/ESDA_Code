library(sf)
library(rstudioapi)
library(dplyr)
library(tmap)
library(viridisLite)

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


# Actual Plotting
tmap_mode("plot")

# Change bounding box
# Get current bounding box
bbox_new = st_bbox(income_join) 

# Get range of x and y
xrange = bbox_new$xmax - bbox_new$xmin
yrange = bbox_new$ymax - bbox_new$ymin

# Modify each direction
# bbox_new[1] = bbox_new[1] - (0.25 * xrange) # xmin - left
# bbox_new[3] = bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] = bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] = bbox_new[4] + (0.2 * yrange) # ymax - top

# Change bbox to sf polygon
bbox_new = st_as_sfc(bbox_new)

# 
income_layer =
  tm_shape(income_join, bbox = bbox_new)+
  tm_fill(col = "Income", alpha = 0.7, palette = "inferno",
          style = "quantile", legend.show = FALSE)

charging_ports_layer =
  tm_shape(charging_ports_in_lsoa) +
  tm_dots(col = "blue", size = 0.00002)

legends = 
  tm_add_legend(type = "symbol", col = c(inferno(5), "blue"),
                border.col = "black", border.lwd = 0.2,size = 1,
                shape = c(22, 22, 22, 22, 22, 21),
                labels = c("2,241 to 15,259",
                           "15,259 to 16,438",
                           "16,438 to 17,584",
                           "17,584 to 19,319",
                           "19,319 to 51,116",
                           "EV Charger"),
                title = "Individual Income")

datasource = 
  tm_credits("Source: \nOffice for National Statistics \nNational Chargepoint Registry",
             size = 0.5, position = c("left", "bottom"))

map =
  income_layer+
  charging_ports_layer+
  legends+
  datasource+
  tm_layout(legend.show = TRUE, 
            legend.position = c(0.05, 0.65),
            legend.title.fontface = "bold",
            title = "EV Charging Port VS Income",
            title.position = c("center", "top")) +
  tm_logo("Energy Institute Logo.png", height = 2, position = c("left", "top")) +
  tm_scale_bar(position = c("right", "bottom"), width = 0.25) +
  tm_compass(position = c("right", "top"), size = 2.5)

tmap_save(map, filename = "EV Charging Port VS Income v1.5.png", width = 1440, height = 1920)
