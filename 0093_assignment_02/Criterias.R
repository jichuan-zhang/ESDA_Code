library(rstudioapi)
library(sf)
library(terra)
library(dplyr)
library(tmap)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Read and do initial processing of citerias

indonesia = st_read("idn_adm_bps_20200401_shp/idn_admbnda_adm0_bps_20200401.shp")
indonesia_sv = vect(indonesia)


# Normalise values of the rasters
raster_template = rast( resolution = 0.01,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)
raster_template2 = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)


wind_speed = rast("IDN_wind-speed_100m.tif")
wind_speed_resample = resample(wind_speed, raster_template, method = "bilinear")
wind_speed_resample = mask(wind_speed_resample, indonesia_sv)

solar_generation = rast("power_output.tif")

#roads = st_read("IDN_rds/IDN_roads.shp")
#distance_to_road = distance(raster_template2, roads)
#writeRaster(distance_to_road, "distance_road_raster.tif")
distance_to_road = rast("distance_road_raster.tif")
distance_to_road_resample = resample(distance_to_road, raster_template, method = "bilinear")
distance_to_road_resample = mask(distance_to_road_resample, indonesia_sv)

#grids = st_read("grid.geojson")
#grids = st_transform(grids, crs = 4326)
#distance_to_grid = distance(raster_template2, grids)
#writeRaster(distance_to_grid, "distance_grid_raster.tif")
distance_to_grid = rast("distance_grid_raster.tif")
distance_to_grid_resample = resample(distance_to_grid, raster_template, method = "bilinear")
distance_to_grid_resample = mask(distance_to_grid_resample, indonesia_sv)

writeRaster(wind_speed_resample, "wind_speed_resample.tif", overwrite=TRUE)
writeRaster(distance_to_road_resample, "distance_to_road_resample.tif", overwrite=TRUE)
writeRaster(distance_to_grid_resample, "distance_to_grid_resample.tif", overwrite=TRUE)





# Wind Speed Normalization (Reversed Mapping)
wind_speed_cutoff = 5
wind_speed_normalised = ifel(wind_speed_resample <= wind_speed_cutoff,
                             5 - ((wind_speed_resample / wind_speed_cutoff) * 5),
                             0)
wind_speed_normalised = mask(wind_speed_normalised, indonesia_sv)
writeRaster(wind_speed_normalised, "wind_speed_normalised.tif", overwrite=TRUE)

# Solar Generation Normalization
solar_generation_cutoff = 0.4
solar_generation_max = 0.63453
solar_generation_normalised = ifel(solar_generation >= solar_generation_cutoff,
                                   (5 * (solar_generation - solar_generation_cutoff) /
                                      (solar_generation_max - solar_generation_cutoff)),
                                   0)
solar_generation_normalised = mask(solar_generation_normalised, indonesia_sv)
writeRaster(solar_generation_normalised, "solar_generation_normalised.tif", overwrite=TRUE)

# Distance to Road Normalization (Reversed Mapping)
distance_to_road_cutoff = 50000
distance_to_road_normalised = ifel(distance_to_road_resample <= distance_to_road_cutoff,
                                   5 - ((distance_to_road_resample / distance_to_road_cutoff) * 5),
                                   0)
distance_to_road_normalised = mask(distance_to_road_normalised, indonesia_sv)
writeRaster(distance_to_road_normalised, "distance_to_road_normalised.tif", overwrite=TRUE)

# Distance to Grid Normalization (Reversed Mapping)
distance_to_grid_cutoff = 150000
distance_to_grid_normalised = ifel(distance_to_grid_resample <= distance_to_grid_cutoff,
                                   5 - ((distance_to_grid_resample / distance_to_grid_cutoff) * 5),
                                   0)
distance_to_grid_normalised = mask(distance_to_grid_normalised, indonesia_sv)
writeRaster(distance_to_grid_normalised, "distance_to_grid_normalised.tif", overwrite=TRUE)

constrained_area = rast("constrained_area.tif")

AHP = (0.236416*distance_to_grid_normalised+
         0.158498*distance_to_road_normalised+
         0.544595*solar_generation_normalised+
         0.060492*wind_speed_normalised)*
  constrained_area

annual_solar_generation = solar_generation*365*1210000

cumulative_generation = 0
required_generation = 481e9
selected_cells = data.frame()

gen_df = as.data.frame(annual_solar_generation, xy = TRUE, na.rm = TRUE)
ahp_df = as.data.frame(AHP, xy = TRUE, na.rm = TRUE)

# Merging data frames based on coordinates
merged_df = merge(gen_df, ahp_df, by = c("x", "y"))

# Sorting by AHP score
sorted_df = merged_df[order(-merged_df$lyr.1),]

for (row in 1:nrow(sorted_df)) {
  cumulative_generation = cumulative_generation + sorted_df$power_output[row]
  selected_cells = rbind(selected_cells, sorted_df[row, ])
  if (cumulative_generation >= required_generation) {
    break
  }
}

# Extract coordinates of selected cells
selected_coords = selected_cells[, c("x", "y")]

# Convert coordinates to SpatialPoints
points_sp = st_as_sf(data.frame(selected_coords), coords = c("x", "y"), crs = st_crs(annual_solar_generation))

# Extract values from the third raster using these points
extracted_values = extract(distance_to_grid_resample, points_sp)

# Sum up the values from the third raster
sum_distance_to_grid = sum(extracted_values, na.rm = TRUE)

average_capacity = sum(selected_cells$power_output)*10/8760/1000/nrow(selected_coords)
average_distance_to_grid = sum_distance_to_grid/nrow(selected_coords)/10

AHP= mask(AHP, indonesia_sv)
writeRaster(AHP, "AHP.tif", overwrite=TRUE)
st_write(points_sp, "points_sp.gpkg")
