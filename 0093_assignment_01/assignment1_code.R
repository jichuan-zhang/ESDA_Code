library(sf)
library(rstudioapi)
library(dplyr)
library(tidyr)
library(tmap)
library(viridisLite)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Read data
charging_ports = read.csv("national-charge-point-registry_modified.csv")
lsoa_boundaries = st_read("LSOA_(Dec_2011)_Boundaries_Generalised_Clipped_(BGC)_EW_V3.geojson")
income = read.csv("Gross_Individual_Income_LSOA.csv")

# Processing the charging port data by removing empty location and convert into sf
missing_rows = which(is.na(charging_ports$longitude) | is.na(charging_ports$latitude))
charging_ports = charging_ports[-missing_rows, ]
charging_ports_sf = st_as_sf(charging_ports, coords = c("longitude", "latitude"), crs = 4326)

# Filter out charging port NOT in wales/england
charging_ports_in_lsoa = st_filter(charging_ports_sf, lsoa_boundaries)

# Join income data with LSOA geometry
# Although doesn't matter, inner makes most sense here because missing on either side should be omitted
lsoa_all = inner_join(lsoa_boundaries, income, join_by("LSOA11CD" == "LSOA.code"))

# Count how many charging port per LSOA
# Assign charging ports to LSOA
charging_ports_in_lsoa = st_join(charging_ports_in_lsoa, lsoa_all, join = st_within)
# Count them
charging_ports_count = summarize(
  group_by(charging_ports_in_lsoa, LSOA11CD),
  count = n())
# Convert the sf to dataframe for left join, also removing geometry column to avoid conflict
charging_ports_count_df = as.data.frame(charging_ports_count)
charging_ports_count_df = charging_ports_count_df[, -3]
# Add the charging port count data to the sf
lsoa_all = left_join(lsoa_all, charging_ports_count_df, by = c("LSOA11CD" = "LSOA11CD"))
# Calculate income per charging port, intensionally not removing NA
lsoa_all = mutate(lsoa_all, income_per_charging_port = Income / count)
lsoa_all = mutate(lsoa_all, area_per_charging_port = Shape__Area / count)
# Save as geojson
lsoa_all = lsoa_all %>%
  select(-LSOA11NMW, -BNG_E, -BNG_N, -LONG, -LAT, -GlobalID, -LSOA.name, -Shape__Length)
st_write(lsoa_all, "assignment_1_lsoa.geojson")
st_write(charging_ports_in_lsoa, "charging_ports_in_lsoa.geojson")
