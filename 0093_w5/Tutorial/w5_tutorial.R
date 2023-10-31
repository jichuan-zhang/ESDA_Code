library(sf)
library(spdep) #key function used for conducting spatial autocorrelation
library (plyr)
library(dplyr)
library(tmap)
library(sp)
library(rstudioapi)


england <- st_read("LSOA.geojson" )
fuel_poverty <- read.csv(  "fuel_poverty_london.csv") 
london <- england[which (england$LSOA11CD %in% fuel_poverty$LSOA_2011_Code ) , ] #extract London lsoa: find out which England lsoa code are within the list of fuel_poverty London's lsoa code.
#extract London lsoa: find out which England lsoa code are within the list of fuel_poverty London's lsoa code.

#join lsoa london and fuel poverty data
london_fp <- left_join(london, fuel_poverty, by=c("LSOA11CD"=  "LSOA_2011_Code"))