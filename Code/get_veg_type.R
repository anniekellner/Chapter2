#####################################################
##    LAND COVER  ###################################
#####################################################

# Categorize vegetation type. 
# Column we want is 'COMM' - reference codes to 'aga_arctic_ak_geobotanical_side1.jpg' 
# https://arcticatlas.geobotany.org/catalog/dataset/alaska-arctic-tundra-vegetation-map-raynolds-et-al-2006

library(sf)
library(dplyr)

rm(list = ls())

# ------------ Load data  -------------------------------- #

land <- st_read('C:/Users/akell/Documents/PhD/Polar_Bears/Data/veg/aga_arctic_ak_geobotanical.shp') # veg shp
land <- st_transform(land, 3338)

land <- select(land, COMM:geometry)

corr <- readRDS('./Data/Derived-data/corridor_data.Rds')
bone <- readRDS('./Data/Derived-data/bonepile_data.Rds')


# ----------  Extract Values ------------------------------------------ #

corr <- st_intersection(corr, land)





