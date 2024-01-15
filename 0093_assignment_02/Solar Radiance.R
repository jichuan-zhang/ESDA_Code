library(ncdf4)
library(raster)
library(rstudioapi)
library(lattice)
library(RColorBrewer)
library(sf)
library(tmap)
library(gstat)
library(terra)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

Solar_Radiance = nc_open("Solar Radiance.nc")

lon = ncvar_get(Solar_Radiance, "longitude")
lat = ncvar_get(Solar_Radiance, "latitude")

ssrd_array = ncvar_get(Solar_Radiance, "ssrd")

dlname = ncatt_get(Solar_Radiance,"ssrd","long_name")
dunits = ncatt_get(Solar_Radiance,"ssrd","units")
fillvalue = ncatt_get(Solar_Radiance,"ssrd","_FillValue")

ssrd_slice = apply(ssrd_array, c(1, 2), mean)

length(na.omit(as.vector(ssrd_slice))) / length(as.vector(ssrd_slice))

image(ssrd_slice, col = rev(brewer.pal(10,"RdBu")))

lonlat = as.matrix((expand.grid(lon, lat)))
ssrd_vec = as.vector(ssrd_slice)

ssrd_df = data.frame(cbind(lonlat, ssrd_vec))
colnames(ssrd_df) = c("lon", "lat", "ssrd")
ssrd_df_value = na.omit(ssrd_df)

ssrd_sf = st_as_sf(ssrd_df_value, coords = c("lon", "lat"))
st_crs(ssrd_sf) = 4326
ssrd_sf = st_transform(ssrd_sf, 4326)

radiation_to_power <- function(radiation, area, yield_r=0.175, pr=0.6, hours=1)
{ kWh <- radiation * area * yield_r * pr * hours * (1/3600000)
return(kWh) }

ssrd_kwh <- as.data.frame (radiation_to_power (ssrd_df_value, 1))
ssrd_df_value <- cbind(ssrd_df_value,ssrd_kwh$ssrd)
colnames(ssrd_df_value) [4] <- 'ssrd_kwh'
ssrd_sf$ssrd_kwh = ssrd_kwh$ssrd

indonesia = st_read("idn_adm_bps_20200401_shp/idn_admbnda_adm0_bps_20200401.shp")

ssrd_sf = st_transform(ssrd_sf, 4326)
indonesia = st_transform(indonesia, st_crs(ssrd_sf))

coor = as.data.frame(st_coordinates(ssrd_sf))
ssrd_sf$x = coor$X
ssrd_sf$y = coor$Y
ssrd_nogeom = st_drop_geometry(ssrd_sf) #get rid of geometry but keep all other attributes
ssrd_nogeom=na.omit(ssrd_nogeom)

gs <- gstat(formula=ssrd~1, locations=~x+y, data=ssrd_nogeom, nmax=Inf, set=list(idp=2))


indonesia = st_read("idn_adm_bps_20200401_shp/idn_admbnda_adm0_bps_20200401.shp")
st_bbox(indonesia)

raster_template = rast( resolution = 0.01,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)

idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra
plot(idw$var1.pred)

idw_mask <- mask(idw, indonesia)
plot(idw_mask$var1.pred)

names(idw_mask) = c( "predicted","observed" )

tm_shape(idw_mask$predicted) + 
  tm_raster(col="predicted", style = "quantile", n = 10, palette= "-YlGn", legend.show = TRUE)

solar_radiance_layer = idw_mask[["predicted"]]

# Assuming solar_radiance_layer is a SpatRaster
solar_radiance_df <- as.data.frame(solar_radiance_layer, xy = TRUE, na.rm=FALSE)
solar_radiance_df$power_output = radiation_to_power(solar_radiance_df$predicted, 1)

# Create an empty raster with the same dimensions and extent as the original
power_output_raster <- raster(solar_radiance_layer)

solar_radiance_df$predicted <- NULL
# Assign the calculated values back to the raster
values(power_output_raster) <- solar_radiance_df$power_output

# Set the CRS if it was lost during conversion (replace 'CRS_string' with the actual CRS)
crs(power_output_raster) <- crs(solar_radiance_layer)

# Write the original predicted layer
writeRaster(solar_radiance_layer, "solar_radiance.tif", overwrite = TRUE)

# Write the processed layer
writeRaster(power_output_raster, "power_output.tif", overwrite = TRUE)


