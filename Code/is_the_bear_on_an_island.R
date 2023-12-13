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

ua <- readRDS(here("Data", "Derived-data", "DFs", "OG", "uaSF_12-12-23.Rds"))

ua <- ua %>% # delete previous columns for on_island
  select(-c(isl, on_island))

islands <- st_read(here("Data", "Spatial", "Barrier_Islands", "islands_w_1500m_buffer.shp")) 


# ------- Plot  ---------------------------------------------------------- #

tmap_mode('view')

tm_shape(islands) + 
  tm_polygons(col = "green") 


# ------  Analysis  ----------------------------------------------------- #

## Intersection

on_island <- lengths(st_intersects(ua, islands)) > 0 # creates a vector of whether or not point is on island

ua <- cbind(ua, on_island)

# Check  

sample1 <- slice_sample(ua, prop = .01, replace = FALSE) # randomly select 1% of observations

tm_shape(islands) + 
  tm_polygons(fill = "green") + 
  tm_shape(sample1) + 
  tm_symbols(size = 0.25, col = "on_island") # looks good 12-13-23


# ----------- Save data ------------------------------ #

#saveRDS(ua, here("Data", "Derived-data", "DFs", "OG", "uaSF_12-13-23.Rds")) # save as spatial object
