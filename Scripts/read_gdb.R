## Packages ----
library(sf)
library(tmap)
library(here)
library(dplyr)

## Data ingest ----
gdb_path <- "./parcel_data/Defensible_Space_2024_address_points.gdb"
layers <- st_layers(gdb_path)
print(layers$name)  # list all the layers in the geodatabase

## Explore ----

data <- st_read(gdb_path, layer = "T2024_Defensible_Space_LatLong_Addr")
data <- st_read(gdb_path, layer = "T2024_Defensible_Space_LatLong_Addr_Points")
data <- st_read(gdb_path, layer = "c2024_Defensible_Space_LatLong_Addr_XYTableToPoint")

## Vizualize ----
tmap_mode("view")
map <- tm_shape(data) +
    tm_dots() 
map

## master list of addresses to merge ----

dta <- st_read(gdb_path, layer = "T2024_Defensible_Space_LatLong_Addr")
uni.addr <- unique(dta$address) %>% data.frame() %>%
    rename(address = 1)

