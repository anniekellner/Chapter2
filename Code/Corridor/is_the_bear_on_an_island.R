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

corr <- readRDS('./Data/Derived-data/corridor_data.Rds')
corr.sf <- st_as_sf(corr, crs = 3338)

islands <- st_read('./Data/Spatial/Barrier_Islands/all_islands.shp') # shapefile with all islands

# ------- Plot  ---------------------------------------------------------- #

plot(st_geometry(corr.sf))
plot(st_geometry(islands), col = "green", add = TRUE)

# ------  Add 5 km buffer to islands ------------------------------------ #

# Use st_cast to convert multipolygons into polygons

cast <- st_cast(islands, "POLYGON")

buff <- st_buffer(cast, dist = 5000)

# Plot to check

tmap_mode('view')

tm_shape(buff) +
  tm_polygons(col = "orange") + 
  tm_shape(cast) + 
  tm_polygons(col = "purple")

# Join

islands_w_buffer <- st_join(cast, buff)

# Save because it takes time to create these buffers

st_write(islands_w_buffer, './Data/Spatial/islands_w_5km_buffer.shp')


# ------  Analysis  ----------------------------------------------------- #

on_island = lengths(st_intersects(corr.sf, islands)) > 0 # creates a vector of whether or not point is on island

ua <- cbind(ua, on_island)

# Send to ArcGIS to check

arcgis <- 'C:/Users/akell/Documents/ArcGIS/Projects/Chapter2/Test' # to 'ground-truth' points in ArcGIS

sample5 <- slice_sample(ua, prop = .5, replace = FALSE) # randomly select 5% of observations
st_write(sample5, paste0(arcgis, "/", "islands_test_nb.shp")) # Looks good 2-14-22

saveRDS(ua, './Data/Derived-data/NB_UA_data.Rds')
