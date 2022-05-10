########################################################
###   DISTANCE TO MAINLAND COAST   #####################
########################################################

## Only calculating distance to coast for mainland points (decided at meeting with GW 4/13/22)
## This metric is really only relevant for mainland points, anyway, and the positive/negative denotation meses with coefficients

# Coast = mainland coast. Digitized using combo of shapefile and IfSAR.

library(dplyr)
library(sf)
library(tmap)
library(tmaptools)


rm(list = ls())

# ----- Load data ---------------------------------- #

coast <- st_read('./Data/Spatial/coastline/digitized_coastline.shp') # Used .shp of coastline as digitized from IFSAR images on GEE
coast <- st_transform(coast, 3338)

bone <- readRDS('./Data/Derived-data/bonepile_data.Rds')
corr <- readRDS('./Data/Derived-data/corridor_data.Rds')

# Create sf objects 

bone.sf <- st_as_sf(bone, coords = c('x_', 'y_'), crs = 3338)
corr.sf <- st_as_sf(corr, crs = 3338)




# --------  Calculate distance to coast for used points ------------------------- #

corr.sf <- corr.sf %>%
  mutate(dist_to_coast = st_distance(., coast)) # will not work without . 

bone.sf <- bone.sf %>%
  mutate(dist_to_coast = st_distance(., coast))

# Check using distance tool in ArcGIS

arcgis <- 'C:/Users/akell/Documents/ArcGIS/Projects/Chapter2/Test'

sample1 <- slice_sample(corr.sf, prop = .01, replace = FALSE) # randomly select 1% of observations
st_write(sample1, paste0(arcgis, "/", "dist_to_coast.shp"), append = FALSE)

bone_sample1 <- slice_sample(bone.sf, prop = .01, replace = FALSE)

# ------  Plot  ---------------------------------------------------------------- #

tmap_mode('view')

tm_shape(coast) + 
  tm_lines(col = "green") + 
  tm_shape(sample1) + 
  tm_symbols(popup.vars = c('dist_to_coast', 'on_island', 'in_water'))

# ----- Save  ------------------------------------------------------------------ #

saveRDS(bone.sf, "./Data/Derived-data/bonepile_data.Rds")










