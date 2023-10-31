library(sf)
library(spdep)
library(rgdal)
library (plyr)
library(tmap)
library(rstudioapi)
library(RColorBrewer)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
wd=getwd()
borough<-readOGR(wd,"crime")

neighbours <- poly2nb(borough)
neighbours 

plot(borough, border = 'black')
plot(neighbours, coordinates(borough), cex=.7, add=TRUE, col='red' )

listw <- nb2listw(neighbours, style="W") #style can take values “W”, “B”, “C”, “U”, “minmax” and “S”
listw

globalMoranI <- moran.test(borough$Crimes, listw)
globalMoranI 
globalMoranI$estimate[1]

#A plot of spatial data against its spatially lagged values, augmented by reporting the summary of influence measures for the linear relationship between the data and the lag.
mor <- moran.plot(borough$Crimes, listw = nb2listw(neighbours, style = "B"),
                    xlab = 'Crime incidents at borough level',
                    ylab = 'Lagged crime incidents') 
View(mor)
moran.plot(as.vector(scale(borough$Crimes))   , listw = nb2listw(neighbours, style = "B"),
                    xlab = 'Crime incidents at borough level',
                    ylab = 'Lagged crime incidents',
                    zero.policy = TRUE)
?moran.plot

lag<- lag.listw(nb2listw(neighbours, style = "B"), borough$Crimes)
plot(borough$Crimes, lag)

local <- localmoran(borough$Crimes, listw)

borough$lm = local[,1]
borough$crimes_meandiff = borough$Crimes - mean(borough$Crimes) 
borough$moran_mean = local[,1]-mean(local[,1])
borough$sig = local[,5]

tm_shape(borough)+
  tm_fill(col="lm", styles="jens", palette = "Spectral")

tm_shape(borough)+
  tm_fill(col="sig", breaks=c(0, 0.05, 0.1, 0.2, 1), palette = "-Blues", alpha=.75)

display.brewer.all()

#define clustering
borough[which (borough$crimes_meandiff >0 & borough$moran_mean >0), "clu"] ="high-high"
borough[which (borough$crimes_meandiff <0 & borough$moran_mean <0), "clu"] ="low-high"
borough[which (borough$crimes_meandiff >0 & borough$moran_mean <0), "clu"] ="high-low"
borough[which (borough$crimes_meandiff <0 & borough$moran_mean >0), "clu"] ="low-low"

tm_shape(borough)+
  tm_fill(col='clu',alpha=.6,
          title= 'clustering of local moran I')+tm_text("NAME", size=.7)


#get rid of insignificant groups: group number 0
borough[which (borough$sig>0.1), "clu"] = 'not significant' #assign the cluster number 0 to those local moran i test showing insignificant results. Question: Why we do this in the last step not the first step? 
one=borough[which(borough$clu !='not significant'),]

tm_shape(borough)+
  tm_fill(col='clu',alpha=.6,
          title= 'clustering of local moran I')+
  tm_shape(one)+tm_borders(col="red")+
  tm_text("NAME", size=.7)



