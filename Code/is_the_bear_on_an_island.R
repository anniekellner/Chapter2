################################################################
####      IS THE BEAR ON AN ISLAND?   ##########################
################################################################

## Add a 1.5 km buffer to islands to account for GPS error and shoreline changes

library(dplyr)
library(sf)
library(tmap)
library(tmaptools)

rm(list= ls())

# --------- Load data ------------------------------------------------------- #

corr <- readRDS('./Data/Derived-data/corridor_data.Rds') # reads in as sf

bone <- readRDS('./Data/Derived-data/bonepile_data.Rds') # reads in as sf

#bone_pts <- readRDS('./Data/all_bonepile_points.Rds') # used only - bonepile
#corr_pts <- readRDS('./Data/all_non_bonepile_pts.Rds') # used only - corridor

islands <- st_read('./Data/Spatial/Barrier_Islands/all_islands_v2.shp') # shapefile with all islands, updated 5/11/22

# ------- Plot  ---------------------------------------------------------- #

tmap_mode('view')

tm_shape(islands) + 
  tm_polygons(col = "green") + 
  tm_shape(corr) + 
  tm_symbols(alpha = 0.25)

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

st_write(buff, './Data/Spatial/islands_w_1500m_buffer.shp')


# ------  Analysis  ----------------------------------------------------- #

## Corr pts

on_island_corr = lengths(st_intersects(corr, buff)) > 0 # creates a vector of whether or not point is on island

corr <- corr %>%
  select(-on_island) %>% 
  cbind(on_island_corr) %>%
  rename(on_island = on_island_corr)

# Check  

sample5 <- slice_sample(corr, prop = .05, replace = FALSE) # randomly select 5% of observations

tm_shape(buff) + 
  tm_polygons(col = "green") + 
  tm_shape(sample5) + 
  tm_symbols(size = 0.25, popup.vars = "on_island") # looks good

## Bone pts
  
on_island_bone = lengths(st_intersects(bone, buff)) > 0

bone <- bone %>%
  select(-on_island) %>%
  cbind(on_island_bone) %>%
  rename(on_island = on_island_bone)

# Check

sample5 <- slice_sample(bone, prop = .05, replace = FALSE) # randomly select 5% of observations

tm_shape(buff) + 
  tm_polygons(col = "green") + 
  tm_shape(sample5) + 
  tm_symbols(size = 0.25, popup.vars = "on_island") # looks good

# ----------- Save data ------------------------------ #

saveRDS(bone,'./Data/Derived-data/bonepile_data.Rds')
saveRDS(corr, './Data/Derived-data/corridor_data.Rds')
