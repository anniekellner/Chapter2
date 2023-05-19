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

bone <- readRDS('./Data/Derived-data/bonepile_data.Rds')

bone.sf <- st_as_sf(bone, coords = c('x_', 'y_'), crs = 3338)

# Visual check - looks good

plot(st_geometry(bone.sf))
plot(st_geometry(islands), col = "green", add = TRUE)

on_island = lengths(st_intersects(bone.sf, islands)) > 0 # creates a vector of whether or not point is on island

bone <- cbind(bone, on_island)
bone.sf <- cbind(bone.sf, on_island) # to check in ArcGIS

# Send to ArcGIS to check

sample1 <- slice_sample(bone.sf, prop = .01, replace = FALSE) # randomly select 1% of observations
st_write(sample1, paste0(arcgis, "/", "islands_test.shp"))

saveRDS(bone, file = './Data/Derived-data/bonepile_data.Rds') # save sf dataframe with column for on island
