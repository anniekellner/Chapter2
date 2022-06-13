#################################################################
###     CREATE CURRENT ENERGY LAYERS    #########################
#################################################################

library(sf)
library(dplyr)
library(tidyr)

# Compare Hilcorp and Conoco to NSSI
# Cannot merge Hilcorp and Conoco into a single shapefile

rm(list = ls())

## Load Conoco-Phillips

cp <- list.files(path = './Data/Spatial/Industry_GIS/CP_Infrastructure', pattern = "[.]shp$", full.names = TRUE)

cp_shp <- list()

for(i in 1:length(cp)){
  shp = st_read(cp[i])
  shp = st_transform(shp, 3338)
  cp_shp[[i]] = shp
}

cp2 <- bind_rows(cp_shp)

cp2 <- cp2 %>%
  drop_na(geometry)

plot(st_geometry(cp2))

## Load Hilcorp

hil <- list.files(path = './Data/Spatial/Industry_GIS/Hilcorp', pattern = "[.]shp$", full.names = TRUE)

hil_shp <- list()

for(i in 1:length(hil)){
  shp = st_read(hil[i])
  shp = st_transform(shp, 3338)
  hil_shp[[i]] = shp
}

hil2 <- bind_rows(hil_shp)

plot(st_geometry(hil2))
plot(st_geometry(cp2), add = TRUE)

## Load NSSI

# Airports and Runways

runways <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/alaska-airports-and-runways-faa/Transportation - Airports and Runways - FAA_LINE.shp')
airports <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/alaska-airports-and-runways-faa/Transportation - Airports and Runways - FAA_POINT.shp')

runways <- st_transform(runways, 3338)
airports <- st_transform(airports, 3338)

plot(st_geometry(runways))
plot(st_geometry(airports), add = TRUE) # Need to crop

bb <- st_read('./Data/Spatial/Bounding_box/rectangle.shp')

runways <- st_crop(runways, bb)
airports <- st_crop(airports, bb)

plot(st_geometry(runways))
plot(st_geometry(airports), add = TRUE)

st_write(runways, './Data/Derived-data/Spatial/NSSI/runways.shp') 
st_write(airports, './Data/Derived-data/Spatial/NSSI/airports.shp')
