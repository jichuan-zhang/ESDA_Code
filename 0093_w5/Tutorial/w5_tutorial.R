library(sf)
library(spdep) #key function used for conducting spatial autocorrelation
library (plyr)
library(dplyr)
library(tmap)
library(sp)
library(rstudioapi)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

england <- st_read("LSOA.geojson" )
fuel_poverty <- read.csv(  "fuel_poverty_london.csv") 
#extract London lsoa: find out which England lsoa code are within the list of fuel_poverty London's lsoa code.
london <- england[which (england$LSOA11CD %in% fuel_poverty$LSOA_2011_Code ) , ] 
#join lsoa london and fuel poverty data
london_fp <- left_join(london, fuel_poverty, by=c("LSOA11CD"=  "LSOA_2011_Code"))

tmap_mode("view")

tm_shape(london_fp )+
  tm_fill(col='X2017', style = "quantile", alph= .4, title= '% of fuel poverty')+
  tm_borders(alpha=.2) 

neighbours <- poly2nb(london_fp)  #default setting is queen contiguity
neighbours

#get the coordinates of each losa's centroids
coords <- st_coordinates(st_centroid(st_geometry(london_fp)))

plot(london_fp$geometry, border = 'pink')
plot(neighbours, coords ,cex=.1, add=TRUE, col='blue')

#knearneigh
kn1 <- knn2nb(knearneigh(coords, k = 1))
kn1

kn2 <- knn2nb(knearneigh(coords, k = 2))
kn3 <- knn2nb(knearneigh(coords, k = 3)) #each lsoa finds three nearest neighbours

z=knearneigh(coords, k = 1)
head(z$nn, 5) #so you can tell the which lsoa is each lsoa's first nearest neighbour.

#if you change the number of k, it will help to find out the id of three nearest neighbour.
head (knearneigh(coords, k = 3)$nn , 5)    

plot(london_fp$geometry, border = 'red')
plot(kn2, coords, add=TRUE, cex=.02, col='blue')

#nbdists() measures distane between neighbours
dist <- unlist(nbdists(kn1, coords))  #calculate max distance of first nearest neighbour
summary(dist) 

max_k1 <- max(dist)  

kd1 <- dnearneigh(coords, d1 = 0, d2 = 0.4 * max_k1)
# if the distance between two borough centroids is withing the range from  (0 to 1.5*max_k1), we say: they are neighbours, 
# if the distance is above 1.5*max_k1, then they are too further away to interact with each other, so they are not neighbours.

#the smaller the distance indicates my criterion is more strict, so fewer lsoa will be selected as my neighbour(s)
plot(london_fp$geometry, border = 'red')
plot(kd1, coords, add=TRUE, cex=.02, col='blue')

#Step 2_define weight matrix
listw <- nb2listw(neighbours, style="W") #style can take values “W”, “B”, “C”, “U”, “minmax” and “S”
listw$weights

kn_all <- knn2nb(knearneigh(coords, k = 4)) #find nearest 4 neighbours
dists <- nbdists(kn_all, coords)
dists

idw <- lapply(dists, function(x) 100/(x))  
listw_idw <- nb2listw(kn_all, glist = idw, style = "W")
listw_idw$weights

london_fp$lag_fp2017 = lag.listw(listw, london_fp$X2017) 
#note: the listw data is the one we created in step 1 and 2

globalMoranI <- moran.test(london_fp$X2017, listw)
globalMoranI 

globalMoranI$estimate[1]

local <- localmoran(x = london_fp$X2017, listw)
# each lsoa will have a local moran i indicator to tell whether it is similar to disimlar to its surroundings
head(local,5) 

mean_2017 = mean(london_fp$X2017)
mean_2017_lag = mean(london_fp$lag_fp2017)
plot(london_fp$X2017, london_fp$lag_fp2017)
abline(v = mean_2017,h= mean_2017_lag, col="red", lwd=3, lty=3)

#red dahsed lines indicate average and we use these lines to decide high and low

moran <- moran.plot(london_fp$X2017, listw = nb2listw(neighbours, style = "W")) 

london_fp$sig = local[,5]
london_fp$cluster=0

#hh
london_fp[ which((london_fp$X2017 - mean_2017) >0 & (london_fp$lag_fp2017-mean_2017_lag) >0),'cluster' ]="H-H"
#ll
london_fp[ which((london_fp$X2017 - mean_2017) <0 & (london_fp$lag_fp2017-mean_2017_lag) <0),'cluster']="L-L"
#lh
london_fp[ which((london_fp$X2017 - mean_2017) <0 & (london_fp$lag_fp2017-mean_2017_lag) >0),'cluster'] ="L-H"
#hl
london_fp[ which((london_fp$X2017 - mean_2017) >0 & (london_fp$lag_fp2017-mean_2017_lag) <0),'cluster' ] ="H-L"
tm_shape(london_fp)+tm_fill(col="cluster")

london_fp[ which(london_fp$sig >0.1),'cluster' ]="Missing"
tm_shape(london_fp)+
  tm_fill(col="cluster",
          palette = c("H-H"= "#7F58AF",
                      "L-L"= "#64C5EB",
                      "L-H"= "#E84D8A",
                      "H-L"= "#FEB326",
                      "Missing"= "#fefae0"),alpha=.7)+ 
  tm_borders(col="white", lwd=0.2)


# different way to do the map
london_fp$fp_mean = london_fp$X2017 - mean(london_fp$X2017)
london_fp$moran_mean = local[,1]-mean(local[,1])

london_fp[which(london_fp$fp_mean > 0 & london_fp$moran_mean > 0 ), "clustering"] = "H-H" 

london_fp[which(london_fp$fp_mean < 0 & london_fp$moran_mean < 0 ), "clustering"] = "L-H"

london_fp[which(london_fp$fp_mean > 0 & london_fp$moran_mean < 0 ), "clustering"] = "H-L"

london_fp[which(london_fp$fp_mean < 0 &london_fp$moran_mean > 0 ), "clustering"] = "L-L"
#not significant group: group number 0
london_fp[which(london_fp$sig >0.1), "clustering"] = "Missing" 
#assign the cluster number 0 to those local moran i test showing insignificant results. 
# Question: Why we do this in the last step not the first step? 

london_fp$clustering <- as.factor(london_fp$clustering ) #just want to emphase clustering column is categorical data instead of numerical.


tm_shape(london_fp)+
  tm_fill(col='clustering',alpha=.6,
          palette = c("H-H"= "#7F58AF",
                      "L-L"= "#64C5EB",
                      "L-H"= "#E84D8A",
                      "H-L"= "#FEB326",
                      "Missing"= "#fefae0"),
          title= 'clustering of local moran I')

#the map should be the same as what we have done in Section 5.3

tm_shape(london_fp)+
  tm_fill(col="sig", breaks=c(0,0.01, 0.05, 0.1, 0.2, 1), palette = "-Greens", alpha=.75)

