#########################################################
####    EXPLORE CONOCO-PHILLIPS   #######################
#########################################################

## Look at files provided by Conoco-Phillips via Heather Johnson

library(sf)
library(tmap)
library(tmaptools)


rm(list = ls())

setwd('C:/Users/akell/Documents/ArcGIS/Polar_Bears_GIS/GIS_from_Todd/Industry_GIS/CP_Infrastructure/CP_Infrastructure') #  When working from laptop

## Badami-Thompson Pipeline

bt_pipe <- st_read('./Badami_Thomson_pipeline.shp')
plot(st_geometry(bt_pipe))

## Haul Road

haul_road <- st_read('./haul_road.shp')
plot(st_geometry(haul_road))

#### ---- KUPARUK -------------  ####

## Additional 'gravel' shapefile looks like additional gravel pads. Not very many, no need to ask. 

gravel <- st_read('./Kuparuk_Gravel.shp')
plot(st_geometry(gravel))

gravel_pads <- st_read('./Kuparuk_Gravel_Pads.shp')
plot(st_geometry(gravel_pads))

gravel_roads <- st_read('./Kuparuk_Gravel_Roads.shp')
plot(st_geometry(gravel_roads))

pipelines <- st_read('./Kuparuk_Pipelines.shp')

roads <- st_read('./Kuparuk_Roads.shp')

# Plot all Kuparuk together

tmap_mode('view')

tm_shape(gravel) +
  tm_polygons(col = "red") + 
  tm_shape(gravel_pads) +
  tm_polygons(col = "gray") + 
  tm_shape(gravel_roads) +
  tm_polygons(col = "blue") + 
  tm_shape(pipelines) + 
  tm_lines(col = "black") +
  tm_shape(roads) + 
  tm_lines(col = "yellow")