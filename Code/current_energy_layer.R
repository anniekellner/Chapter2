#################################################################
###     CREATE CURRENT ENERGY LAYERS    #########################
#################################################################

library(sf)
library(dplyr)
library(tidyr)
library(maps)
library(ggplot2)
library(googleway) # can pull geographic coords from Google
theme_set(theme_bw())
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)


# Compare Hilcorp and Conoco to NSSI
# Cannot merge Hilcorp and Conoco into a single shapefile

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

faa <- st_read('./Data/Derived-data/Spatial/NSSI/NS_pipes_roads.shp') # why is this file so large?

airports <- st_read('./Data/Derived-data/Spatial/NSSI/airports.shp')
runways <- st_read('./Data/Derived-data/Spatial/NSSI/runways.shp')

faa <- bind_rows(airports, runways)

# Roads, pipelines, and developed areas

NSpipes <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/North_slope_infrastructure_roads_pipelines_developed_areas/NSPiplines_V10.shp')
NSRoads <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/North_slope_infrastructure_roads_pipelines_developed_areas/NSRoads_V10.shp')
NSDev <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/North_slope_infrastructure_roads_pipelines_developed_areas/NSDevAreas_V10.shp')

NSpipes$NAME <- "NA"
NSpipes$Route_Name <- "NA"
NSpipes$Unit_Name <- "NA"

ns <- bind_rows(NSpipes, NSRoads) # Not including development areas - ask Todd whether these should be included

#st_write(ns, './Data/Derived-data/Spatial/NSSI/NS_pipes_roads.shp') # Takes a bit of time to combine 

transak <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/trans_alaska_pipeline/Transportation - Pipelines - Trans Alaska Pipeline System_LINE.shp')

# ------  PLOT TO SEE DIFFERENCES BTW CP/HILCORP AND NSSI   ------------------------------ #

sf_use_s2(FALSE) # Otherwise get errors; https://github.com/r-spatial/sf/issues/1856

## Background maps

world <- ne_countries(scale = "medium", returnclass = "sf") # rnaturalearth package
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

# Get city coords from Google

key <- "AIzaSyDSOPDGRNBNE-ZoLj4PM608-dpDrQ0VNgg" # Google API key

akcities <- data.frame(state= rep("Alaska", 3), city = c("Niuqsut", "Deadhorse", "Kaktovik"))

coords <- apply(akcities, 1, function(x){
  google_geocode(address = paste(x["city"], x["state"], sep = ", "), 
                 key = key)
})

akcities <- cbind(akcities, do.call(rbind, lapply(coords, geocode_coordinates)))

akcities <- st_as_sf(akcities, coords = c("lng", "lat"), remove = FALSE, # Convert to SF format
                     crs = 4326, agr = "constant")

# Oil and gas data

cp2 <- st_as_sf(cp2, coords = c("longitude", "latitude"), # Needs to be in lat/long to be compatible with ggplot features
                crs = 4326, agr = "constant")

hil2 <- st_as_sf(hil2, coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")

faa <- st_as_sf(faa, coords = c("longitude", "latitude"), 
                crs = 4326, agr = "constant")

ns <- st_as_sf(ns, coords = c("longitude", "latitude"), 
               crs = 4326, agr = "constant")

## ggplot

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = akcities) +
  geom_text_repel(data= akcities, aes(x = lng, y = lat, label = city),
            size = 3.0, fontface = "bold", nudge_y = c(-0.25, -0.25, 0.25)) + # package ggrepel = flexible approach to label placement
  geom_sf(data = cp2, color = "red") +
  geom_sf(data = hil2, color = "red") +
  geom_sf(data = faa, color = "blue") +
  geom_sf(data = ns, color = "green") +
  coord_sf(xlim = c(-152, -143), ylim = c(70.0,70.8), expand = FALSE)






ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_sf(data = cp2, color = "red") +
  geom_sf(data = hil2, color = "red") +
  geom_sf(data = faa, color = "blue") +
  geom_sf(data = ns, color = "green") +
  coord_sf(xlim = c(-156, -141), ylim = c(69,71), expand = FALSE)

