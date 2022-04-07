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

cast_test <- st_cast(examp, "POLYGON")

plot(st_geometry(cast_test))

buff_cast <- st_buffer(cast_test, dist = 5)

# See if it works

tm_shape(buff_cast) + 
  tm_polygons(col = "orange") + 
  tm_shape(cast_test) + 
  tm_polygons(col = "purple")


# Take subsample so doesn't take so long to plot while figuring this out

examp <- dplyr::filter(islands, Shape_Area < 5000)

buff_ex <- sf::st_buffer(examp2, dist = 5)

tmap_mode('view')

tm_shape(cast_test) + 
  tm_polygons()

buff <- sf::st_buffer(islands, dist = 5, endCapStyle = "ROUND")

# Plot to check

tmap_mode('view')

tm_shape(buff) +
  tm_polygons(col = "orange") + 
  tm_shape(islands) + 
  tm_polygons(col = "purple")

# ------  Analysis  ----------------------------------------------------- #

on_island = lengths(st_intersects(ua, islands)) > 0 # creates a vector of whether or not point is on island

ua <- cbind(ua, on_island)

# Send to ArcGIS to check

arcgis <- 'C:/Users/akell/Documents/ArcGIS/Projects/Chapter2/Test' # to 'ground-truth' points in ArcGIS

sample5 <- slice_sample(ua, prop = .5, replace = FALSE) # randomly select 5% of observations
st_write(sample5, paste0(arcgis, "/", "islands_test_nb.shp")) # Looks good 2-14-22

saveRDS(ua, './Data/Derived-data/NB_UA_data.Rds')
