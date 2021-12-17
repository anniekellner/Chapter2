###############################################
###   DISTANCE TO COAST   #####################
###############################################

# Used .shp of coastline as digitized from IFSAR images on GEE

library(sf)
library(tmap)
library(tmaptools)

rm(list = ls())

# Load data

coast <- st_read('./Data/Spatial/coastline/digitized_coastline.shp')
coast <- st_transform(coast, 3338)

pts <- readRDS('./Data/all_bonepile_points.Rds')

# Plot

tmap_mode('view')

tm_shape(coast) + 
  tm_lines(col = "green") + 
  tm_shape(pts) + 
  tm_dots()

