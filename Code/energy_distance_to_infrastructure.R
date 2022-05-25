################################################
#####   ENERGY: DISTANCE TO INFRASTRUCTURE  ####
################################################

# Combine all energy infrastructure into a single entity and calculate distance to it

library(sf)
library(dplyr)

rm(list = ls())

# Read in all .shp files from a directory

cp <- list.files(path = './Data/Spatial/Industry_GIS/CP_Infrastructure', pattern = "[.]shp$", full.names = TRUE)
fac <- list.files(path = './Data/Spatial/Industry_GIS/Facilities', pattern = "[.]shp$", full.names = TRUE)
