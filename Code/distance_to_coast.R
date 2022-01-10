###############################################
###   DISTANCE TO MAINLAND COAST   #####################
###############################################

# Coast = mainland coast
# Distance: water --> land = -
#           land --> water = +  

library(dplyr)
library(sf)
library(tmap)
library(tmaptools)

rm(list = ls())

arcgis <- 'C:/Users/akell/Documents/ArcGIS/Projects/Chapter2/Test'

# Load data

coast <- st_read('./Data/Spatial/coastline/digitized_coastline.shp') # Used .shp of coastline as digitized from IFSAR images on GEE
coast <- st_transform(coast, 3338)

pts <- readRDS('./Data/Derived-data/bonepile_pts_used_avail.Rds')
pts <- st_as_sf(pts, coords = c('x_', 'y_'), crs = 3338) # convert to sf

# Plot

#tmap_mode('view')

#tm_shape(coast) + 
  #tm_lines(col = "green") + 
  #tm_shape(pts) + 
  #tm_dots()

# Calculate distance for coast for used points

dist_to_coast <- pts %>%
  mutate(dist_to_coast = st_distance(., coast)) # will not work without . 

# Check values in ArcGIS - look good 

#sample10 <- slice_sample(dist_to_coast, prop = .10, replace = FALSE) # randomly select 10% of observations
#st_write(sample10, paste0(arcgis, "/", "dist_to_coast.shp"))

#saveRDS(pts, './Data/Derived-data/bonepile_data_used_avail.Rds')

         