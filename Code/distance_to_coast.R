########################################################
###   DISTANCE TO MAINLAND COAST   #####################
########################################################

## Only calculating distance to coast for mainland points (decided at meeting with GW 4/13/22)
## This metric is really only relevant for mainland points
## The positive/negative denotation messes with coefficients

# Coast = mainland coast. Digitized using combo of shapefile and IfSAR.

library(dplyr)
library(sf)
library(tmap)
library(tmaptools)


rm(list = ls())

source('./Code/MyFunctions.R')

# ----- Load data ---------------------------------- #

coast <- st_read('./Data/Spatial/coastline/digitized_coastline.shp') # Used .shp of coastline as digitized from IFSAR images on GEE
coast <- st_transform(coast, 3338)

bone <- readRDS('./Data/Derived-data/bonepile_data.Rds')
corr <- readRDS('./Data/Derived-data/corridor_data.Rds')


# --------  Calculate distance to coast  ------------------------- #

corr <- corr %>%
  st_as_sf() %>%
  st_set_crs(3338) 
  
corr <- corr %>%
  mutate(dist_to_coast = st_distance(., coast)) # will not work without . 
  
bone.sf <- bone.sf %>%
  mutate(dist_to_coast = st_distance(., coast))

# --------  Replace points in water & on islands with NA ---------- #

corr2 <- corr %>%
  mutate(dist_to_coast = ifelse(in_water == 1 | on_island == TRUE, NA_character_, dist_to_coast))

bone2 <- bone %>%
  mutate(dist_to_coast = ifelse(in_water == 1 | on_island == TRUE, NA_character_, dist_to_coast))

# Check using distance tool in ArcGIS
# Looks good

arcgis <- 'C:/Users/akell/Documents/ArcGIS/Projects/Chapter2/Test'

sample_corr <- slice_sample(corr2, prop = .01, replace = FALSE) # randomly select 1% of observations
st_write(sample_corr, paste0(arcgis, "/", "dist_to_coast_test_corr.shp"), append = FALSE)

sample_bone <- slice_sample(bone2, prop = .01, replace = FALSE)
st_write(sample_bone, paste0(arcgis, "/", "dist_to_coast_test_bone.shp"), append = FALSE)

# ------  Plot  ---------------------------------------------------------------- #

tmap_mode('view')

tm_shape(coast) + 
  tm_lines(col = "green") + 
  tm_shape(sample1) + 
  tm_symbols(popup.vars = c('dist_to_coast', 'on_island', 'in_water'))

# ----- Save  ------------------------------------------------------------------ #

saveRDS(bone2, "./Data/Derived-data/bonepile_data.Rds")
saveRDS(corr2, "./Data/Derived-data/corridor_data.Rds")









