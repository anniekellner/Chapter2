#####################################################
##    LAND COVER  ###################################
#####################################################

# Categorize vegetation type. 
# Column we want is 'COMM' - reference codes to 'aga_arctic_ak_geobotanical_side1.jpg' 
# https://arcticatlas.geobotany.org/catalog/dataset/alaska-arctic-tundra-vegetation-map-raynolds-et-al-2006

library(sf)
library(dplyr)
library(tmap)
library(tmaptools)

rm(list = ls())

# ------------ Load data  -------------------------------- #

land <- st_read('C:/Users/akell/Documents/PhD/Polar_Bears/Data/veg/aga_arctic_ak_geobotanical.shp') # veg shp

bb <- st_read('./Data/Spatial/Bounding_box/rectangle.shp')

corr <- readRDS('./Data/Derived-data/corridor_data.Rds')
bone <- readRDS('./Data/Derived-data/bonepile_data.Rds')


# Prep Data

land <- st_transform(land, 3338)
land <- select(land, COMM:geometry)

bb <- st_bbox(bb)

land <- st_crop(land, bb)


# ----------  Extract Values ------------------------------------------ #

corr <- st_intersection(corr, land)
bone <- st_intersection(bone, land)

# -----   Plot  -------------------------------------------------------- #

tmap_mode('view')

tm_shape(land) + 
  tm_polygons(col = "COMM") + 
  tm_layout(legend.outside = TRUE) + 
  tm_shape(corr) + 
  tm_symbols(size = 0.5,popup.vars = c("COMM"))

# LOOKS GOOD!





