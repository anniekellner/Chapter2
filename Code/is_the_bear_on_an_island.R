############################################################################
###   Is the bear on an island?   ##########################################
############################################################################

# Consider trying a buffer around the point to account for ephemeral nature of land 

library(sf)
library(tmap)
library(tmaptools)
library(dplyr)

rm(list = ls())

# Load data

islands <- st_read('./Data/Spatial/Barrier_Islands/all_islands.shp') # shapefile with all islands

all <- readRDS('./Data/ssf_1hr_sf.Rds') # sf object with all points - true and false

# Visual check - looks good

plot(st_geometry(all))
plot(st_geometry(islands), col = "green", add = TRUE)

on_island = lengths(st_intersects(all, islands)) > 0 # creates a vector of whether or not point is on island

all <- cbind(all, on_island)

saveRDS(all, file = './Data/ssf_1hr_sf_11122021.Rds') # save sf dataframe with column for on island
