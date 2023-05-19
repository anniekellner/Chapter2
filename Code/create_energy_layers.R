#############################################################
#######   CREATE ENERGY SHAPEFILES  #########################
#############################################################

library(tidyverse)
library(sf)

rm(list = ls())

# --------- LOAD DATA ---------------------------------------------- #

## Load Conoco-Phillips

cp <- list.files(path = './Data/Spatial/Industry_GIS/CP_Infrastructure', pattern = "[.]shp$", full.names = TRUE)

cp_shp <- list()

for(i in 1:length(cp)){
  shp = st_read(cp[i])
  shp = st_transform(shp, 3338)
  cp_shp[[i]] = shp
}

cp2 <- bind_rows(cp_shp)

#saveRDS(cp2, file = "./Data/Derived-data/Spatial/cp_all.Rds") # save as RDS file because cannot save .shp with multiple geometry


## Load Hilcorp

hil <- list.files(path = './Data/Spatial/Industry_GIS/Hilcorp', pattern = "[.]shp$", full.names = TRUE)

hil_shp <- list()

for(i in 1:length(hil)){
  shp = st_read(hil[i])
  shp = st_transform(shp, 3338)
  hil_shp[[i]] = shp
}

hil2 <- bind_rows(hil_shp)

#saveRDS(hil2, './Data/Derived-data/Spatial/hil_all.Rds')

## Load NSSI

airports <- st_read('./Data/Derived-data/Spatial/NSSI/airports.shp')
runways <- st_read('./Data/Derived-data/Spatial/NSSI/runways.shp')

faa <- bind_rows(airports, runways)

#saveRDS(faa, './Data/Derived-data/Spatial/NSSI/faa.Rds')

# Roads, pipelines, and developed areas

NSpipes <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/North_slope_infrastructure_roads_pipelines_developed_areas/NSPiplines_V10.shp')
NSRoads <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/North_slope_infrastructure_roads_pipelines_developed_areas/NSRoads_V10.shp')
#NSDev <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/North_slope_infrastructure_roads_pipelines_developed_areas/NSDevAreas_V10.shp')

NSpipes$NAME <- "NA"
NSpipes$Route_Name <- "NA"
NSpipes$Unit_Name <- "NA"

ns <- bind_rows(NSpipes, NSRoads) # Not including development areas - ask Todd whether these should be included

#st_write(ns, './Data/Derived-data/Spatial/NSSI/NS_pipes_roads.shp') # Takes a bit of time to combine 