################################################################
####      IS THE BEAR ON AN ISLAND?   ##########################
################################################################

## Add a 5 km buffer to islands to account for GPS error and shoreline changes

library(dplyr)
library(sf)
library(tmap)
library(tmaptools)

rm(list= ls())

# --------- Load data ------------------------------------------------------- #

corr <- readRDS('./Data/Derived-data/corridor_data.Rds') # used + available
corr.sf <- st_as_sf(corr, crs = 3338)

bone <- readRDS('./Data/Derived-data/bonepile_data.Rds') # reads in as sf

#bone_pts <- readRDS('./Data/all_bonepile_points.Rds') # used only - bonepile
#corr_pts <- readRDS('./Data/all_non_bonepile_pts.Rds') # used only - corridor

islands <- st_read('./Data/Spatial/Barrier_Islands/all_islands.shp') # shapefile with all islands

# ------- Plot  ---------------------------------------------------------- #

plot(st_geometry(corr.sf))
plot(st_geometry(islands), col = "green", add = TRUE)

# ------  Add buffer to islands ------------------------------------ #

# Use st_cast to convert multipolygons into polygons

cast <- st_cast(islands, "POLYGON")

buff <- st_buffer(cast, dist = 1500)

# Plot to check

tmap_mode('view')

tm_shape(buff) +
  tm_polygons(col = "orange") + 
  tm_shape(cast) + 
  tm_polygons(col = "purple") + 
  tm_shape(bone_pts) + 
  tm_symbols(size = 0.25) + 
  tm_shape(corr_pts) + 
  tm_symbols(size = 0.1, col = "black") + 
  tm_shape(bone_pts) + 
  tm_symbols(size = 0.1, col = "gray")

# Save because it takes time to create these buffers

#st_write(buff, './Data/Spatial/islands_w_1500m_buffer.shp')


# ------  Analysis  ----------------------------------------------------- #

on_island = lengths(st_intersects(corr.sf, buff)) > 0 # creates a vector of whether or not point is on island
on_island = lengths(st_intersects(bone, buff)) > 0

ua_bone <- cbind(bone, on_island)

# Send to ArcGIS to check

arcgis <- 'C:/Users/akell/Documents/ArcGIS/Projects/Chapter2/Test' # to 'ground-truth' points in ArcGIS

sample5 <- slice_sample(ua, prop = .5, replace = FALSE) # randomly select 5% of observations
st_write(sample5, paste0(arcgis, "/", "islands_test_nb.shp")) # Looks good 2-14-22

saveRDS(ua_bone, './Data/Derived-data/bonepile_data.Rds')
