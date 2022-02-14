################################################################
####      IS THE BEAR ON AN ISLAND?   ##########################
################################################################

library(dplyr)
library(sf)

rm(list= ls())

# --------- Load data ------------------------------------------------------- #

ua <- readRDS('./Data/Derived-data/non_bp_ua_sf.Rds')

islands <- st_read('./Data/Spatial/Barrier_Islands/all_islands.shp') # shapefile with all islands

# ------- Plot  ---------------------------------------------------------- #

plot(st_geometry(ua))
plot(st_geometry(islands), col = "green")

# ------  Analysis  ----------------------------------------------------- #

on_island = lengths(st_intersects(ua, islands)) > 0 # creates a vector of whether or not point is on island

ua <- cbind(ua, on_island)

# Send to ArcGIS to check

arcgis <- 'C:/Users/akell/Documents/ArcGIS/Projects/Chapter2/Test' # to 'ground-truth' points in ArcGIS

sample5 <- slice_sample(ua, prop = .5, replace = FALSE) # randomly select 5% of observations
st_write(sample5, paste0(arcgis, "/", "islands_test_nb.shp")) # Looks good 2-14-22

saveRDS(ua, './Data/Derived-data/NB_UA_data.Rds')
