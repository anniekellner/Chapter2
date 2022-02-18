###############################
##    EXTRACT TERRAIN DATA  ###
###############################

# Data from shapefiles created using Google Earth Engine

library(sf)
library(dplyr)

rm(list = ls())

# ---- Load data  ----------------------- #

# GEE
corr.gee <- st_read('C:/Users/akell/OneDrive - Colostate/PhD/Chapter2/Data/Terrain/terrain_corridor_pts.shp')
bone.gee <- st_read('C:/Users/akell/OneDrive - Colostate/PhD/Chapter2/Data/Terrain/terrain_bonepile_pts.shp')

# R

corr <- readRDS('Data/Derived-data/corridor_data.Rds')
bone <- readRDS('Data/Derived-data/bonepile_data.Rds')

# --- Join --------------------------- #

# Convert into dataframes

corr.gee <- as.data.frame(corr.gee)
bone.gee <- as.data.frame(bone.gee)

corr <- as.data.frame(corr)
bone <- as.data.frame(bone)

# Get rid of extraneous columns

corr.gee <- select(corr.gee, elevation, aspect, Point_ID, slope)
bone.gee <- select(bone.gee, elevation, aspect, Point_ID, slope)

# Join by Point_ID

corr2 <- left_join(corr, corr.gee)
bone2 <- left_join(bone, bone.gee)

# Save

saveRDS(corr2, './Data/Derived-data/corridor_data.Rds')
saveRDS(bone2, './Data/Derived-data/bonepile_data.Rds')
