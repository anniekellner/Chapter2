#################################################################
###     CREATE CURRENT ENERGY LAYERS    #########################
#################################################################

# It is much easier to compare layers in ArcGIS over R

library(tidyverse)
library(sf)
library(maps)
library(googleway) # can pull geographic coords from Google
theme_set(theme_bw())
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

rm(list = ls())

# --------- LOAD DATA ---------------------------------------------- #

ns <- st_read('./Data/Derived-data/Spatial/NSSI/NS_pipes_roads.shp')
ns <- st_transform(ns, 3338)

transak <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/trans_alaska_pipeline/Transportation - Pipelines - Trans Alaska Pipeline System_LINE.shp')

hil <- readRDS('./Data/Derived-data/Spatial/hil_all.Rds')
cp <- readRDS('./Data/Derived-data/Spatial/cp_all.Rds')


# ------------ CHECK AGAINST TODD EMAIL --------------------------------------- #

## Compare NSSI to CP/Hilcorp Industry data

# Combine CP and Hilcorp

cp$OPERATOR <- "CP"

cp <- cp %>%
  select(NAME, OPERATOR, Type, LABEL, geometry) %>%
  rename(TYPE = Type)

hil <- hil %>%
  select(NAME, OPERATOR, TYPE, LABEL, geometry)

hil$TYPE <- as.character(hil$TYPE) # Change from integer to character in order to use bind_rows()

ind <- bind_rows(cp, hil)

# Get intersection

inter <- st_intersection(ind, ns, tolerance = 1000) # Looked at distance between Hilcorp shp and NS shp in Arcmap. Very rough estimate. 


plot(st_geometry(inter))
plot(st_geometry(ns), col = "red", add = TRUE) # Can manually assess differences because there are not too many. 

plot(st_geometry(ns), col = "red", add = TRUE)
plot(st_geometry(ind), col = "blue", add = TRUE)

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
# Not sure this works or is necessary...

cp2 <- st_as_sf(cp, coords = c("longitude", "latitude"), # Needs to be in lat/long to be compatible with ggplot features
                crs = 4326, agr = "constant")

hil2 <- st_as_sf(hil, coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")

faa <- st_as_sf(faa, coords = c("longitude", "latitude"), 
                crs = 4326, agr = "constant")

ns2 <- ns %>% 
  st_as_sf(ns, coords = c("longitude", "latitude"), 
               crs = 4326, agr = "constant")

## ggplot

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = akcities) +
  geom_text_repel(data= akcities, aes(x = lng, y = lat, label = city),
            size = 3.0, fontface = "bold", nudge_y = c(-0.25, -0.25, 0.25)) + # package ggrepel = flexible approach to label placement
  geom_sf(data = inter, color = "red") +
  geom_sf(data = ns, color = "blue") +
  coord_sf(xlim = c(-152, -143), ylim = c(70.0,70.8), expand = FALSE)





plot(st_geometry(ind))
plot(st_geometry(ns)), add = TRUE)

# Intersect industry with NSSI

inter <- st_intersection(ind, )

# Combine cp and hil




## Exclude ice roads

iceroad <- st_read('C:/Users/akell/Documents/ArcGIS/GIS from Todd/Industry GIS/OIL/IceRoads/iceroad.shp') %>%
  st_set_crs(4326)

npra_icerd <- st_read('C:/Users/akell/Documents/ArcGIS/GIS from Todd/Industry GIS/OIL/IceRoads/npra_2000_2001.shp') %>%
  st_set_crs(4326)

iceroad <- st_as_sf(iceroad, coords = c("longitude", "latitude"), # Needs to be in lat/long to be compatible with ggplot features
                crs = 4326, agr = "constant")

npra_icerd <- st_as_sf(npra_icerd, coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = akcities) +
  geom_text_repel(data= akcities, aes(x = lng, y = lat, label = city),
                  size = 3.0, fontface = "bold", nudge_y = c(-0.25, -0.25, 0.25)) + # package ggrepel = flexible approach to label placement
  geom_sf(data = cp2, color = "red") +
  geom_sf(data = hil2, color = "red") +
  geom_sf(data = faa, color = "blue") +
  geom_sf(data = ns, color = "green") +
  geom_sf(data = iceroad, color = "#C5E9F6") +
  geom_sf(data = npra_icerd, color = "#C5E9F6") +
  coord_sf(xlim = c(-152, -143), ylim = c(70.0,70.8), expand = FALSE)


tmap_mode('view')
tm_shape(iceroad) + 
  tm_lines()

ns %>%
  filter(str_detect(Route_Name, 'thom')) 
