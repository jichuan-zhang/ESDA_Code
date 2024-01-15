library(rstudioapi)
library(sf)
library(terra)
library(RColorBrewer)
library(tmap)
library(dplyr)
library(tmap)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# Read and do initial processing of constriants

indonesia = st_read("idn_adm_bps_20200401_shp/idn_admbnda_adm0_bps_20200401.shp")

elevation = rast("IDN_msk_alt/IDN_msk_alt.vrt")

protect_poly_1 = st_read("WDPA_WDOECM_Jan2024_Public_IDN_shp/WDPA_WDOECM_Jan2024_Public_IDN_shp_0/WDPA_WDOECM_Jan2024_Public_IDN_shp-polygons.shp")
protect_poly_2 = st_read("WDPA_WDOECM_Jan2024_Public_IDN_shp/WDPA_WDOECM_Jan2024_Public_IDN_shp_1/WDPA_WDOECM_Jan2024_Public_IDN_shp-polygons.shp")
protect_poly_3 = st_read("WDPA_WDOECM_Jan2024_Public_IDN_shp/WDPA_WDOECM_Jan2024_Public_IDN_shp_2/WDPA_WDOECM_Jan2024_Public_IDN_shp-polygons.shp")

protect_pt_1 = st_read("WDPA_WDOECM_Jan2024_Public_IDN_shp/WDPA_WDOECM_Jan2024_Public_IDN_shp_0/WDPA_WDOECM_Jan2024_Public_IDN_shp-points.shp")
protect_pt_2 = st_read("WDPA_WDOECM_Jan2024_Public_IDN_shp/WDPA_WDOECM_Jan2024_Public_IDN_shp_1/WDPA_WDOECM_Jan2024_Public_IDN_shp-points.shp")
protect_pt_3 = st_read("WDPA_WDOECM_Jan2024_Public_IDN_shp/WDPA_WDOECM_Jan2024_Public_IDN_shp_2/WDPA_WDOECM_Jan2024_Public_IDN_shp-points.shp")

protect_poly = rbind(protect_poly_1, protect_poly_2, protect_poly_3)
protect_pt = rbind(protect_pt_1, protect_pt_2, protect_pt_3)

land_cover = st_read("ind_gc_adg/ind_gc_adg_1.shp")

raster_template = rast( resolution = 0.01,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)

inclination <- terrain(elevation, v = "slope", unit = 'degrees')
crs(inclination) <- "+proj=longlat +datum=WGS84 +no_defs"

inclination_resample = resample(inclination, raster_template, method = "bilinear")

protect_poly_within = st_intersection(protect_poly, indonesia)
protect_pt_within = st_intersection(protect_pt, indonesia)

writeRaster(inclination_resample, "inclination.tif")
st_write(protect_poly_within, "protect_poly.gpkg", append = FALSE)
st_write(protect_pt_within, "protect_pt.gpkg", append = FALSE)

# Apply Constriants onto the geometry avilable
binary_raster = ifel(inclination_resample > 15, 1, NA)
incline_excl_polygons = as.polygons(binary_raster, dissolve = TRUE)
incline_excl_polygons_sf = st_as_sf(incline_excl_polygons)

st_write(incline_excl_polygons_sf, "incline_excl.gpkg")

urban_area = 190
tree_cover = c(40, 50, 110)

urban_excl_sf = land_cover %>%
  filter(GRIDCODE %in% urban_area)

tree_excl_sf = land_cover %>%
  filter(GRIDCODE %in% tree_cover)

st_write(urban_excl_sf, "urban_excl.gpkg")
st_write(tree_excl_sf, "tree_excl.gpkg")

protect_poly_within <- st_make_valid(protect_poly_within)
urban_excl_sf <- st_make_valid(urban_excl_sf)
tree_excl_sf <- st_make_valid(tree_excl_sf)

protect_poly_within_buffer = st_buffer(protect_poly_within, dist = 0.02)
protect_pt_within_buffer = st_buffer(protect_pt_within, dist = 0.02)
urban_excl_sf_buffer = st_buffer(urban_excl_sf, dist = 0.02)
tree_excl_sf_buffer = st_buffer(tree_excl_sf, dist = 0.02)

sf_list <- list(incline_excl_polygons_sf,
                protect_poly_within,protect_poly_within_buffer,
                protect_pt_within, protect_pt_within_buffer,
                urban_excl_sf, urban_excl_sf_buffer,
                tree_excl_sf, tree_excl_sf_buffer)

sf_list_geometry_only <- lapply(sf_list, function(x) {
  st_sf(geometry = st_geometry(x))
})

combined_excl_sf <- do.call(rbind, sf_list_geometry_only)

st_write(combined_excl_sf, "combined_excl.gpkg")

indonesia_avilable = st_difference(indonesia, combined_excl_sf)

# Normalise values of the rasters
raster_template = rast( resolution = 0.01,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)


wind_speed = rast("IDN_wind-speed_100m.tif")
wind_speed_resample = resample(wind_speed, raster_template, method = "bilinear")

solar_generation = rast("power_output.tif")

roads = st_read("IDN_rds/IDN_roads.shp")
distance_to_road = distance(raster_template, roads)

grids = st_read("grid.geojson")
grids = st_transform(grids, crs = 4326)
distance_to_grid = distance(raster_template, grids)



wind_speed_cutoff = 5
wind_speed_resample[wind_speed_resample > wind_speed_cutoff] = 0
wind_speed_min = 0
wind_speed_normalised = ifel(wind_speed_resample <= wind_speed_cutoff,
                             (5*(wind_speed_cutoff - wind_speed_resample) / wind_speed_cutoff),
                             wind_speed_resample)


solar_generation_cutoff = 
solar_generation[solar_generation < solar_generation_cutoff] = 0
solar_generation_max = max(solar_generation, na.rm = TRUE)
solar_generation_normalised = ifel(solar_generation >= solar_generation_cutoff,
                             (5*(solar_generation - solar_generation_cutoff) /
                                (solar_generation_max - solar_generation_cutoff)),
                             solar_generation)


distance_to_road_cutoff = 
distance_to_road[distance_to_road > distance_to_road_cutoff] = 0
distance_to_road_min = 0
distance_to_road_normalised = ifel(distance_to_road <= distance_to_road_cutoff,
                             (5*(distance_to_road_cutoff - distance_to_road) / distance_to_road_cutoff),
                             distance_to_road)


distance_to_grid_cutoff = 
wind_speed_resample[wind_speed_resample > wind_speed_cutoff] = 0
wind_speed_min = 0
wind_speed_normalised = ifel(wind_speed_resample <= wind_speed_cutoff,
                             (5*(wind_speed_cutoff - wind_speed_resample) /
                                (wind_speed_cutoff - wind_speed_min)),
                             wind_speed_resample)













