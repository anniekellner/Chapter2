################################################################
####      IS THE BEAR ON AN ISLAND?   ##########################
################################################################

## Added a 1.5 km buffer to islands to account for GPS error and shoreline changes

library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(here)
library(conflicted)

rm(list= ls())

# --------- LOAD AND PREP DATA   ------------------------------------------------------- #

ua <- readRDS(here("Data", "Derived-data", "DFs", "OG", "allUA.Rds"))

islands <- st_read(here("Data", "Spatial", "Barrier_Islands", "islands_w_1500m_buffer.shp")) 

# Create spatial object for bear pts

uaSF <- st_as_sf(ua, coords = c('X', 'Y'), crs = 3338)


# ------- Plot  ---------------------------------------------------------- #

tmap_mode('view')

tm_shape(islands) + 
  tm_polygons(col = "green") 

# ------  Add buffer to islands ------------------------------------ #

# Plot to check

tmap_mode('view')

tm_shape(islands) +
  tm_polygons(col = "orange") + 
  tm_shape(uaSF) + 
  tm_symbols(size = 0.25) 

# Save because it takes time to create these buffers

#st_write(buff, './Data/Spatial/islands_w_1500m_buffer.shp')


# ------  Analysis  ----------------------------------------------------- #

## Intersection

on_island <- lengths(st_intersects(uaSF, islands)) > 0 # creates a vector of whether or not point is on island

uaSF <- cbind(uaSF, on_island)

# Check  

sample1 <- slice_sample(uaSF, prop = .005, replace = FALSE) # randomly select 5% of observations

tm_shape(islands) + 
  tm_polygons(col = "green") + 
  tm_shape(sample1) + 
  tm_symbols(size = 0.25, popup.vars = "on_island") # looks good


# ----------- Save data ------------------------------ #

saveRDS(uaSF, here("Data", "Derived-data", "DFs", "OG", "uaSF.Rds")) # save as spatial object
