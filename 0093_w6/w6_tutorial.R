library(sf)
library(terra)
library(spatstat)
library(tmap)
library(dplyr)
library(plotly)
library(gstat)
library(rstudioapi)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# PART 0 #
pts <- read_sf("part0_data/ZMB_pts_and_data.gpkg") #load file
colnames(pts)[1:4] <- c("id","solar" ,"alt", "aspect") #Name columns

# Load rasters
altitude  <- rast("part0_data/cropped_alt_WGS84_to_UTM.tif") #altitude
crs(altitude)= crs(pts)
altitude_c <- crop(altitude, st_bbox(pts))  #crop altitude based on bounding box of points data
resolution <- rast(nrows=220, ncols=100, xmin=822821.1, xmax=1090849, ymin=8422907, ymax=8896059)
crs(resolution) = crs(altitude_c)
altitude_r = resample(altitude_c, resolution )
altitude_r

# Method 1 OLS
ols <- lm(solar~ alt , data=pts) #association between solar radiance and altitude
summary(ols)

plot(solar~ alt , data=pts)
abline(ols)

pts$res=ols$residuals

names(altitude_c) = 'alt'
summary(values(altitude_c))

linear <- predict(ols, newdata = altitude_c, se.fit = TRUE)

altitude_c$predicted = linear$fit
plot(altitude_c$predicted)

tmap_mode("plot")

#do another plot
tm_shape(altitude_c$predicted)+
  tm_raster(palette = "YlOrRd")+ 
  tm_shape(pts)+tm_dots(col= "res",size=.4, palette="PRGn", midpoint =mean(ols$residuals))

df= as.data.frame(altitude_c$predicted, xy=TRUE)

plot_ly(x=df$x,y=df$y,z=df$predicted , type = 'mesh3d',colorscale = 'Viridis', intensity =df$predicted )

# Method 2 Thiessen Polygon

solarV <- vect(pts) #terra::vect create spatial vector data
v <- voronoi(solarV) #create  delaunay triangles and also assign the point value. Note: this function can also help to transfer the points value to each voronoi polygon
plot(v,"solar") #plot triangles
points(solarV) #plot points

#do some crop and make it more clear
V_solar <- crop(v, st_bbox(pts)) #in most of cases, we use crop for dealing with raster data, but crop can also be used for spatial vector data. 
plot(V_solar, "solar")
points(solarV)

st_bbox(pts)

raster_template = rast( resolution = 3000,
                        xmin= 823022.1, ymin=8423161.2 ,xmax=1090506.0, ymax=8895605.0 ,  crs = st_crs(pts)$wkt)
raster_template #EPSG:32735

V_solar.sf = st_as_sf(V_solar)
raster_V_solar = rasterize(V_solar.sf, raster_template, "solar")
plot(raster_V_solar)

# Method 3 IDW
library(gstat)

coor = as.data.frame(st_coordinates(pts))
pts$x = coor$X
pts$y = coor$Y
pts_nogeom = st_drop_geometry(pts)
#pts_nogeom is a data frame with its original attributes and also contains coordinates columns x and y

gs <- gstat(formula=solar~1, locations=~x+y, data=pts_nogeom, nmax=Inf,set=list(idp=2)) #data should be in data frame format
#idp is the distance power 1/d^k, so idp here indicate k value
gs


st_bbox(pts)

raster_template = rast( resolution = 3000,
                        xmin= 823022.1, ymin=8423161.2 ,xmax=1090506.0, ymax=8895605.0 ,  crs = st_crs(pts)$wkt)
raster_template

idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra
plot(idw$var1.pred)

par(mfrow = c(2,3)) # 2row by 3 columns
idp_list= c(1:6)
for (i in 1:6){
  gs <- gstat(formula=solar~1, locations=~x+y, data=pts_nogeom, nmax=Inf, set=list(idp=i)) #data should be in data frame format
  idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra
  plot(idw$var1.pred, main=paste0("IDW interpolation, idp=", idp_list[i])) 
}








