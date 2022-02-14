################################################################
####      IS THE BEAR ON AN ISLAND?   ##########################
################################################################

library(dplyr)
library(sf)

rm(list= ls())

# --------- Load data ------------------------------------------------------- #

ua <- readRDS('./Data/Derived-data/non_bp_ua_sf.Rds')
ua <- st_set_crs(ua, 3338)

#saveRDS(ua, './Data/Derived-data/non_bp_ua_sf.Rds')


islands <- st_read('./Data/Spatial/Barrier_Islands/all_islands.shp') # shapefile with all islands

# ------- Plot  ---------------------------------------------------------- #

plot(st_geometry(ua))
plot(st_geometry(islands), col = "green")

# ------  Analysis  ----------------------------------------------------- #

ua <- ua %>%
  mutate(on_island = st_intersection(., islands))

on_island = lengths(st_intersects(all, islands)) > 0 # creates a vector of whether or not point is on island
