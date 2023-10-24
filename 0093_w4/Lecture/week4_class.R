##week 4_raster and vector data interactions operations

library(rstudioapi)
library(terra)
library(sf)
library(tmap)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
solar = rast("solar_WGS84_Zambia.tif")
zambia = st_read("Zambia.geojson") 
solar

#1) mask: mask raster by vector====
solar_mask= mask(solar, zambia)
plot(solar_mask)

#2) mask_inverse: mask raster by vector====
solar_mask2= mask(solar, zambia, inverse=TRUE)
plot(solar_mask2)


#3) extract raster values by point location====
#a)extract solar values of point (28.32, -15.44)
id = cellFromXY(solar,xy=matrix(c (28.32, -15.44 ), nrow=1 ) )
solar[id]

#b)extract solar values by multiple points====
#create several random points coordinates
n=100
df <- data.frame (lon  = runif(n, min=21.99, max=33.71),
                  lat = runif(n, min=-18.08, max=-8.22)  )
points_sf = st_as_sf(df, coords= c("lon", "lat"), crs=4326)

tm_shape(solar)+
  tm_raster()+
  tm_shape(points_sf) +
  tm_dots()

solar_point = terra::extract(solar, points_sf)
#the output is data frame
points_sf = cbind(points_sf, solar_point) #join the extract value to the point simple features
tm_shape(points_sf) +tm_dots(col="solar_WGS84_Zambia")

#4) extract raster values by a line====
#create a line based on two coordinates
transect = cbind(c(28.3, 31.5), c(-14.7, -13.2))
transect=  st_linestring(transect )  
transect = st_sfc( transect, crs = crs( solar))
transect = st_sf(transect)
tm_shape(solar)+tm_raster()+
  tm_shape(transect)+tm_lines(lwd=2)

#segment lines 

#create a line based on two coordinates
transect = st_segmentize(transect, dfMaxLength =100) 
#st_segmentize :adds points to straight lines
# dfMaxLength max length of a line segment

transect = st_cast(transect, "POINT")
#st_cast: Cast geometry to another type
start_point = transect[1,]

dist = as.data.frame ( as.vector (st_distance( transect,start_point)) )
colnames(dist) = "distance"
transect$dist = dist$distance

#calculate distance between each points and the starting point
solar_transect= terra::extract(solar, transect)
transect$solar = solar_transect$solar_WGS84_Zambia

plot(transect$dist, transect$solar, type= "b")
library(ggplot2)
ggplot(transect, mapping = aes(x = dist, y = solar)) +
  geom_line()+
  xlab("distance (m)") + ylab("solar radiance")


#5) rasterisation: convert a vector to raster====
#rasterise zambia boundary data
st_bbox(zambia)
raster_template = rast( resolution = 0.4,
                        xmin=21.99, ymin=-18.08,xmax=33.71, ymax=-8.22,  crs = st_crs(zambia)$wkt)

plot(raster_template)
#rasterise  polygons
raster_zambia = rasterize(zambia, raster_template)
plot(raster_zambia, col="yellow")
plot(st_geometry(zambia), add=TRUE)
#rasterize function assign the cells touches zambia as 1 and assgin the rest cell as NA

#rasterise line
raster_transect = rasterize(transect, raster_template)

plot(st_geometry(zambia))
plot(transect, add=TRUE)
plot(raster_transect, col="blue", add=TRUE)
#rasterise points
raster_points = rasterize(points_sf, raster_template)
plot(raster_points, col="red")

plot(st_geometry(zambia))
plot(points_sf, add=TRUE)



#6)vectorise====
r_matrix<-matrix (c(2100, 2300, 1,
                    2300, 2400, 2,
                    240, 2600, 3), ncol=3, byrow= TRUE)   #This code chunk indicates that we want to make cell with value between 10-25 into 1, ans cell with value 25-50 into 2, and so on. 
solar_rc <- classify(solar, r_matrix, include.lowest=TRUE ) #use predined matrix to guide the reclassification.

solar_polygon = st_as_sf ( as.polygons(solar_rc ))
plot(solar_polygon)



