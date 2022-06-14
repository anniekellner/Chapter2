#################################################################
###     CREATE CURRENT ENERGY LAYERS    #########################
#################################################################

library(sf)
library(dplyr)
library(tidyr)
library(maps)
library(googleway) # can pull geographic coords from Google
theme_set(theme_bw())

# Compare Hilcorp and Conoco to NSSI
# Cannot merge Hilcorp and Conoco into a single shapefile

rm(list = ls())

key <- "AIzaSyDSOPDGRNBNE-ZoLj4PM608-dpDrQ0VNgg" # Google API key

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

airports <- st_read('./Data/Derived-data/Spatial/NSSI/airports.shp')
runways <- st_read('./Data/Derived-data/Spatial/NSSI/runways.shp')

faa <- st_union(airports, runways)


# Roads, pipelines, and developed areas

NSpipes <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/North_slope_infrastructure_roads_pipelines_developed_areas/NSPiplines_V10.shp')
NSRoads <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/North_slope_infrastructure_roads_pipelines_developed_areas/NSRoads_V10.shp')
NSDev <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/North_slope_infrastructure_roads_pipelines_developed_areas/NSDevAreas_V10.shp')

ns <- st_union(NSpipes, NSRoads) # Not including development areas - ask Todd whether these should be included

transak <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/trans_alaska_pipeline/Transportation - Pipelines - Trans Alaska Pipeline System_LINE.shp')

# Transform to 3338 

NSpipes <- st_transform(NSpipes, 3338)
NSRoads <- st_transform(NSRoads, 3338)
NSDev <- st_transform(NSDev, 3338)
transak <- st_transform(transak, 3338)

# ------  PLOT TO SEE DIFFERENCES BTW CP/HILCORP AND NSSI   ------------------------------ #

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

## ggplot

world <- ne_countries(scale = "medium", returnclass = "sf")
cp2 <- st_as_sf(cp2, coords = c("longitude", "latitude"), 
                  crs = 4326, agr = "constant")

hil2 <- st_as_sf(hil2, coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")

faa <- st_as_sf(faa, coords = c("longitude", "latitude"), 
                crs = 4326, agr = "constant")

ns <- st_as_sf(ns, coords = c("longitude", "latitude"), 
               crs = 4326, agr = "constant")

sf_use_s2(FALSE) # Otherwise get error; https://github.com/r-spatial/sf/issues/1856

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_sf(data = cp2, color = "red") +
  geom_sf(data = hil2, color = "red") +
  geom_sf(data = faa, color = "blue") +
  geom_sf(data = ns, color = "green") +
  coord_sf(xlim = c(-156, -141), ylim = c(69,71), expand = FALSE)

