library(rstudioapi)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#=== week3 class exercises about raster 
# package we will use is {terra}

## install.pacakge("terra")
library(terra)

# a. create a raster data using  rast() and check basic info about raster====

#in addition to set the number of rows and columns, we also need to set a bouding box. 
#To do so, we need bottom and upper right points' coordinates
r=rast(ncols=200, nrows=180,  #the raster is 200 columns and 180 rows
       xmin=-0.5103751, ymin=51.2867602, xmax=0.3340156, ymax=51.6918741 ) 
r #similar to  sf, by typing the name of data,  we can see the basic info about the raster data we created

#What is the length and width of eahc cell in the matrix? 
res (r) #res can help us to check the resolution of each cell in the raster. 
dim(r) #check dimension


# b. assign values to raster cell and check values====
values(r) <- 1:ncell(r)  #the value from the first to the 

#to check the cell value, you can do it in two ways
values(r) #check all values
head(r) #as we have not assign value so the value of the first six is NA.
tail(r) #check the last cells
#the second way to do it is to convert it to dataframe and then check
r.df = as.data.frame(r)
r.df2 = as.data.frame(r, xy=TRUE)

#c. stack rasters and make a raster data compoesed of multiple layers====
r2 = r 
r3 = r #we now have three raster layers

names(r2) #check the name of layer. 
#we can also use names() to rename it
names(r2) = "lyr.2"
names(r3) = "lyr.3"

newdat = c (r, r2, r3) #stack three layers
newdat #check the basic information about the newly created raster. 
#Check the dimension and nanmes in particular

# d. plot raster data====
library(tmap)
tmap_mode("view") # "view" or "plot"

tm_shape(r)+ #if it is one layer raster, we do not need to specify the layer
  tm_raster(col="lyr.1", n=50, alpha=0.7) 

tm_shape(newdat$lyr.2)+ #if it is a multiple-layer raster, we do need to specify the layer
  tm_raster(col="lyr.2", n=10, alpha=0.7)

#==== e. raster data subset by id index, by nrow ncol=====
r[400] 
r[2, 200]

#what if I am curious about one pair of coordinates value? we can also use coordinates to query value
id = cellFromXY(r,xy=matrix(c (0.33, 51.69), nrow=1 ) )
r[id]

# f. raster resampling ====
#as we said, the data size of raster is not fixed, we can increase and also decrease.

#two ways to achieving resampling 
#f.1 disagg(), aggregate ()
#disaggregate:to increase resolution, more cells
r_disagg <- disagg(r, fact=3, method = "bilinear" ) #method can be Either "near" for nearest or "bilinear" for bilinear interpolation
# Bilinear resampling is another resampling method. In bilinear resampling, each new raster cell gets a weighted average of four nearest cells from the input, rather than just one.
dim (r_disagg) #540 * 600
dim(r)         #180 * 200

#aggregate data: to decrease resolution, fewer cells
r_agg <- terra::aggregate(r, fact=3, method = "mean")   #function can be "mean", "max", "min", "median", "sum" and "modal"
dim (r_agg) #60 * 67

#f.2 resample ()
#say this time we want to make r_disagg the same resolution as r_agg
r_res = resample(r_disagg, r_agg)
dim (r_res)  #60 * 67
#we can also do it the other way around
r_res2 = resample( r_agg,r_disagg)
dim (r_res2) #540 * 600
?terra::resample #check how different method resample data
#the best part of using resample is that when we have multiple raster data, we can reasmple them and ensure they are in the same resolution.

# g. read raster data ====
# rast function can not create raster but also read in raster data
solar = rast( "solar_WGS84_Zambia.tif")
solar
plot(solar)
#review what we have learned check other information about the imported raster
head(solar)
values(solar)
res(solar)
size(solar)
names(solar) = "solar"


# h. reclassify value=====
#this is about solar data. What if we want to reclassify the areas into three groups: low, medium and high.
summary(values(solar))
hist(values(solar))
#define a matrix
r_matrix<-matrix (c(2102, 2250, 1, #range between 2100-2250 is grouped into 1
                    2250, 2350, 2,
                    2350, 2575, 3)
                  , ncol=3, byrow= TRUE) 
r_matrix
#reclassify  (reclassify cell values) based on the new matrix
solar_rc <- classify(solar, r_matrix, include.lowest=TRUE ) 
plot(solar_rc)
res(solar)
plot(solar)

# i recap spatial raster data resampling=====
solar_rs = aggregate(solar, fact= 6, method="mean" )
plot(solar_rs) #can you see the difference? 
plot(solar) 
#plot( aggregate(solar, fact= 20, method="mean" ) )

#use resample()
res(solar)
#what if i want to make res = 0.02
solar2 = solar
res(solar2) = 0.02 #set the new raster -solar2- resolution as what we want, 0.2
solar_rs2 = resample(solar, solar2)
plot(solar_rs2)
solar_rs2 #check the resolution 


# j masking & cropping =====
#This is Zambisa's solar data, what if we only keep the data within Zambia's boundary polygon
library(sf)
zambia = st_read("Zambia.geojson") 
plot(solar)
plot(zambia, col=NA, add=TRUE)

#check coordinate system
#zambia= st_transform(zambia, crs(solar)) #in our examples, two datasets are originially in the same coordinates. 

# use mask()
solar_zambia = mask(solar, zambia)
plot(solar_zambia)

ncell(solar_zambia ) == ncell(solar) #mask does not crop the cells outside Zambia but assign NA to those cells
head(solar_zambia)

# use crop(). You can only crop rectangular areas (see page 66: https://cran.r-project.org/web/packages/terra/terra.pdf)

#define an extent
crop_ext = ext( 24,28,-13,-9)  #order: xmin, xmax, ymin, ymax
solar_zambia2= crop(solar,crop_ext)
plot(solar_zambia2)

# k four different ways to visualise=====
#same as vector data, we will use solar_zambia as an example

#k.1, plot ( )
plot(solar_zambia)

#k.2, tmap( )
library(tmap)
tm_shape(solar_zambia)+
  tm_raster(style="pretty", alpha=.7, n=10)


# k.3 leaflet
library(leaflet)
#for some reasons, leaflet has not yet adapted to terra, if you want to use leaflet to visualise, you may need to re-define solar_zambia
library(raster)
solar_zambia3=raster(solar_zambia)

pal <- colorNumeric(c("#E9090B", "#FB8C07", "#FBF328"), values(solar_zambia3),
                    na.color = "transparent")
#(note: find colour code HEX: https://imagecolorpicker.com/color-code/fbf328)
leaflet() %>% addTiles() %>%
  addRasterImage(solar_zambia3, colors = pal, opacity = 0.8)  %>%
    addLegend(pal = pal, values = values(solar_zambia3),title = "Solar")
                                                             

# k.4 ggplot
library(ggplot2)
#ggplot cannot directly plot spatial raster, we need to extract each cell's coordinates
solar_zambia3.df = as.data.frame(solar_zambia3,xy = TRUE)
head(solar_zambia3.df, 5)

ggplot() +
  geom_raster(data=solar_zambia3.df, aes(x = x, y = y, fill = solar))

solar_zambia3.df2 = na.omit(solar_zambia3.df)
ggplot() +
  geom_raster(data=solar_zambia3.df2, aes(x = x, y = y, fill = solar))


