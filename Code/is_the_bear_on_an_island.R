############################################################################
###   Is the bear on an island?   ##########################################
############################################################################

# Consider trying a buffer around the point to account for ephemeral nature of land 

library(sf)
library(tmap)
library(tmaptools)
library(dplyr)

rm(list = ls())

arcgis <- 'C:/Users/akell/Documents/ArcGIS/Projects/Chapter2/Test' # to 'ground-truth' points in ArcGIS

# Load data

islands <- st_read('./Data/Spatial/Barrier_Islands/all_islands.shp') # shapefile with all islands

all <- readRDS('./Data/Derived-data/bonepile_data_used_avail.Rds') # sf object with all points - true and false

# Visual check - looks good

plot(st_geometry(all))
plot(st_geometry(islands), col = "green", add = TRUE)

on_island = lengths(st_intersects(all, islands)) > 0 # creates a vector of whether or not point is on island

all <- cbind(all, on_island)

# Send to ArcGIS to check

#sample10 <- slice_sample(all, prop = .10, replace = FALSE) # randomly select 10% of observations
#st_write(sample10, paste0(arcgis, "/", "islands_test.shp"))

saveRDS(all, file = './Data/Derived-data/bonepile_data_used_avail.Rds') # save sf dataframe with column for on island
