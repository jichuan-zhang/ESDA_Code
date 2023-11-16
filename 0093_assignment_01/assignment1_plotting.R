library(sf)
library(rstudioapi)
library(tmap)
library(viridisLite)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

charging_ports_in_lsoa = st_read("charging_ports_in_lsoa.geojson")
lsoa_all = st_read("assignment_1_lsoa.geojson")

tmap_mode("plot")

# Change bounding box
# Get current bounding box
bbox_new = st_bbox(lsoa_all) 

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


income =
  tm_shape(lsoa_all, bbox = bbox_new)+
  tm_fill(col = "Income", alpha = 0.7, palette = "inferno",
          style = "quantile", legend.show = FALSE)

charging_ports_count =
  tm_shape(lsoa_all, bbox = bbox_new)+
  tm_fill(col = "count", alpha = 0.7, palette = "inferno",
          breaks = c(1, 2, 3, 4, 7, Inf), 
          legend.show = FALSE)

income_per_charging_port =
  tm_shape(lsoa_all, bbox = bbox_new)+
  tm_fill(col = "income_per_charging_port", alpha = 0.7, palette = "-inferno",
          style = "quantile", legend.show = FALSE)

area_per_charging_port = 
  tm_shape(lsoa_all, bbox = bbox_new)+
  tm_fill(col = "count", convert2density = TRUE, area = "Shape__Area",
          alpha = 0.7, palette = "inferno",
          style = "quantile", legend.show = FALSE)

charging_ports_scatter =
  tm_shape(charging_ports_in_lsoa) +
  tm_dots(col = "blue", size = 0.00002)

legends1 =
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


legends2 = 
  tm_add_legend(type = "symbol", col = c(inferno(5), "grey"),
                border.col = "black", border.lwd = 0.2,size = 1,
                shape = c(22, 22, 22, 22, 22, 22),
                labels = c("1",
                           "2",
                           "3",
                           "4 to 6",
                           "7 or more",
                           "No Charger in LSOA"),
                title = "Income per EV Charger")

legends3 = 
  tm_add_legend(type = "symbol", col = c(rev(inferno(5)), "grey"),
                border.col = "black", border.lwd = 0.2,size = 1,
                shape = c(22, 22, 22, 22, 22, 22),
                labels = c("119 to 3,325",
                           "3,325 to 5,827",
                           "5,827 to 8,874",
                           "8.874 to 16.283",
                           "16.283 to 50,102",
                           "No Charger in LSOA"),
                title = "Income per EV Charger")

legends4 = 
  tm_add_legend(type = "symbol", col = c(rev(inferno(5)), "grey"),
                border.col = "black", border.lwd = 0.2,size = 1,
                shape = c(22, 22, 22, 22, 22, 22),
                labels = c("0 to 1",
                           "1 to 3",
                           "3 to 8",
                           "8 to 21",
                           "21 to 683",
                           "No Charger in LSOA"),
                title = "Area Density / m2")

datasource =
  tm_credits("Source: \nOffice for National Statistics \nNational Chargepoint Registry",
             size = 0.5, position = c("left", "bottom"))

other_stuff =
  tm_logo("Energy Institute Logo.png", height = 2, position = c("left", "top")) +
  tm_scale_bar(position = c("right", "bottom"), width = 0.25) +
  tm_compass(position = c("right", "top"), size = 2.5)

# First Map - Scattered Charging Port on Income
map1 =
  income+
  charging_ports_scatter+
  legends1+
  datasource+
  tm_layout(legend.show = TRUE, 
            legend.position = c(0.05, 0.65),
            legend.title.fontface = "bold",
            title = "Scattered Charging Port",
            title.position = c("center", "top"))+
  other_stuff

# Second Map - Income per EV Charger
map2 =
  charging_ports_count+
  legends2+   
  datasource+
  tm_layout(legend.show = TRUE,
            legend.position = c(0.05, 0.65),
            legend.title.fontface = "bold",
            title = "Number of EV Charging Port",
            title.position = c("center", "top"))+
  other_stuff

# Third Map - Income per EV Charger
map3 =
  income_per_charging_port+
  legends3+   
  datasource+
  tm_layout(legend.show = TRUE,
            legend.position = c(0.05, 0.65),
            legend.title.fontface = "bold",
            title = "Income per EV Charging Port",
            title.position = c("center", "top"))+
  other_stuff

# Fourth Map - Income per EV Charger
map4 =
  area_per_charging_port+
  legends4+   
  datasource+
  tm_layout(legend.show = TRUE,
            legend.position = c(0.05, 0.65),
            legend.title.fontface = "bold",
            title = "EV Charging Port Area Density",
            title.position = c("center", "top"))+
  other_stuff

tmap_save(map4, filename = "Area Density 1.png", width = 1440, height = 1920)
