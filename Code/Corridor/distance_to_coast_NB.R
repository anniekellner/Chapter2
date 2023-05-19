#####################################################################
##########    DISTANCE TO MAINLAND COAST FOR NON-BONEPILE POINTS ####
#####################################################################

# NEED TO SEE WHETHER POINT IS IN WATER SO CAN CALCULATE DISTANCES AS NEGATIVE IF IN WATER

rm(list = ls())

library(dplyr)
library(sf)
library(tmap)
library(tmaptools)
library(amt)

# Load data

coast <- st_read('./Data/Spatial/coastline/digitized_coastline.shp') # Used .shp of coastline as digitized from IFSAR images on GEE
coast <- st_transform(coast, 3338)

pts <- readRDS('./Data/Derived-data/non_bp_ua_sf.Rds')
pts <- st_set_crs(pts, 3338)

# --------  Plot  ------------------------------------------------ #

tmap_mode('view')

tm_shape(coast) + 
  tm_lines(col = "green") + 
  tm_shape(pts) + 
  tm_dots(col = "case")  

# --------  Calculate distance to coast  ------------------------- #

dist <- pts %>%
  mutate(dist_to_coast = st_distance(., coast, by_element = TRUE)) # will not work without . or by_element