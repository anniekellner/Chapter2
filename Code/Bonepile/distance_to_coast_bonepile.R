###############################################
###   DISTANCE TO MAINLAND COAST   #####################
###############################################

# Coast = mainland coast

library(dplyr)
library(sf)
library(tmap)
library(tmaptools)
library(amt)

rm(list = ls())

# Load data

coast <- st_read('./Data/Spatial/coastline/digitized_coastline.shp') # Used .shp of coastline as digitized from IFSAR images on GEE
coast <- st_transform(coast, 3338)

pts <- readRDS('./Data/all_bonepile_points.Rds')

mcp <- readRDS('./Data/Spatial/MCPs/mcp_bonepiles.Rds')

# ------  Plot  ---------------------------------------------------------------- #

tmap_mode('view')

tm_shape(coast) + 
  tm_lines(col = "green") + 
  tm_shape(pts) + 
  tm_dots() + 
  tm_shape(mcp) + 
  tm_polygons(fill = id)


# --------  Calculate distance to coast for used points ------------------------- #

used <- pts %>%
  select(id, geometry) %>%
  mutate(dist_to_coast = st_distance(., coast)) # will not work without . 

#saveRDS(used, './Data/used_bonepile.Rds')





