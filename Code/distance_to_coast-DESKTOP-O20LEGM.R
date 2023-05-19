########################################################
###   DISTANCE TO MAINLAND COAST   #####################
########################################################

# Coast = mainland coast

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

#bone.sf <- st_as_sf(bone, coords = c('x_', 'y_'), crs = 3338)
#corr.sf <- st_as_sf(corr, crs = 3338)

# --------  Calculate distance to coast for used points ------------------------- #

corr <- corr %>%
  mutate(dist_to_coast = st_distance(., coast)) # will not work without . 

bone <- bone %>%
  mutate(dist_to_coast = st_distance(., coast))

# If in the water or on an island, make negative

corr$dist_to_coast <- ifelse(corr$on_island == "TRUE" | corr$in_water == 1, corr$dist_to_coast*-1, corr$dist_to_coast)

bone$dist_to_coast <- ifelse(bone$on_island == "TRUE" | bone$in_water == 1, bone$dist_to_coast*-1, bone$dist_to_coast)

# Check using distance tool in ArcGIS

arcgis <- 'C:/Users/akell/Documents/ArcGIS/Projects/Chapter2/Test'

sample1 <- slice_sample(corr, prop = .01, replace = FALSE) # randomly select 1% of observations
st_write(sample1, paste0(arcgis, "/", "dist_to_coast.shp"), append = FALSE)

bone_sample1 <- slice_sample(bone, prop = .01, replace = FALSE)

# ------  Plot  ---------------------------------------------------------------- #

tmap_mode('view')

tm_shape(coast) + 
  tm_lines(col = "green") + 
  tm_shape(sample1) + 
  tm_symbols(popup.vars = c('dist_to_coast', 'on_island', 'in_water'))

# ----- Save  ------------------------------------------------------------------ #

saveRDS(bone, "./Data/Derived-data/bonepile_data.Rds")










