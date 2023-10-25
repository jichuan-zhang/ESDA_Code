library(sf)
library(rstudioapi)
library(dplyr)
library(tmap)
library(terra)
library(spatstat)
library(ggplot)
library(reshape2)


setwd(dirname(rstudioapi::getSourceEditorContext()$path))

charger <- read.csv('national-charge-point-registry.csv') #25249 chargers
charger = charger[ c(1,4,5)] #only keep charger id and coordinates
charger = charger[-which (is.na(charger$latitude)),] #remove the point with na, otherwise, we can not create point vector data in the next step
head(charger, 3)

charger.sf = st_as_sf(charger, coords = c("longitude", "latitude"), crs=4326) #generate spatial points 
tmap_mode("view")

tm_shape(charger.sf)+tm_dots(size=0.001, col='blue')

borough<-st_read("London_Borough_Excluding_MHW.shp")

borough <- st_transform(borough, 4326) #transfer the projection
#now we use st_filter to select chargers within London borough
charger_london <- st_filter(charger.sf, borough)  #charger_join is the chargers points within London boundary
#to get the charger points within borough, you can also use st_intersection(charger.sf, borough)
# Please also try this code and see what you have got: plot(charger.sf[borough,])

nrow(charger_london)
tm_shape(borough)+tm_borders()+
  tm_shape(charger_london)+tm_dots(size=0.01, col="blue")
