library(tmap)
library(sf)
library(readxl)
library(rstudioapi)
library(dplyr)

# Setting working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the Shapefile
indonesia_country <- st_read("idn_adm_bps_20200401_shp/idn_admbnda_adm0_bps_20200401.shp")


