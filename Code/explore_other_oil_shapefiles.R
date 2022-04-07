##########################################################
####    EXPLORE OTHER ENERGY GIS LAYERS   ################
##########################################################

## Shapefiles from  the Industry GIS layers TA sent
## 

library(sf)
library(tmap)
library(tmaptools)

rm(list = ls())

# Directories

setwd('C:/Users/akell/Documents/ArcGIS/Polar_Bears_GIS/GIS_from_Todd/Industry_GIS') #  When working from laptop

# Industry GIS (from TA)

facilities <- st_read('./Facilities/Faciliites.shp')

### ------ OIL -------------------------- #########

## ICE ROADS

ice_roads <- st_read('./OIL/IceRoads/iceroad.shp')
plot(st_geometry(ice_roads))

## FACILITIES

fac <- st_read('./OIL/FAC.shp')
plot(st_geometry(fac))

fac2_3 <- st_read('./OIL/FAC2_3.shp')
plot(st_geometry(fac2_3))

tm_shape(fac) + 
  tm_lines(col = "green") +
  tm_shape(fac2_3) + 
  tm_lines(col = "blue") + 
  tm_layout(legend.show = TRUE) 

## PIPES
## self-explanatory

pipes <- st_read('./OIL/PIPES.shp')
plot(st_geometry(pipes))

## PROP_FAC

prop_fac <- st_read('./OIL/PROP_FAC.shp')
plot(st_geometry(prop_fac))

## ROADS
roads <- st_read('./OIL/ROADS.shp')
plot(st_geometry(roads))

tiger_roads <- st_read('./OIL/tiger_roads.shp')
tiger_roads <- tiger_roads %>%
  st_set_crs(4326) %>%
  st_transform(4609)

# See difference between roads and tiger_roads

tm_shape(roads) + 
  tm_lines(col = "green") + 
  tm_shape(tiger_roads) +
  tm_lines(col = "blue") + 
  tm_layout(legend.show = TRUE)
