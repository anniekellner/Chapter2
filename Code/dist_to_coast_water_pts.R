#####################################################################
########### DISTANCE TO COAST FOR POINTS IN WATER  ##################
#####################################################################

## To determine appropriate buffer for RSF

library(dplyr)
library(sf)
library(tmap)
library(tmaptools)

rm(list = ls())

# ----- Load data ---------------------------------- #

coast <- st_read('./Data/Spatial/coastline/digitized_coastline.shp') # Used .shp of coastline as digitized from IFSAR images on GEE
coast <- st_transform(coast, 3338)

corr <- readRDS('./Data/Derived-data/corridor_data.Rds')

# --------  Calculate distance to coast for water points ------------------------- #

corr <- corr %>%
  st_as_sf() %>%
  st_set_crs(3338) 

corr <- corr %>%
  filter(case_ == "TRUE") %>%
  mutate(dist_to_coast = st_distance(., coast)) # will not work without . 

corr2 <- corr %>%
  mutate(dist_to_coast = ifelse(in_water == 0, NA_character_, dist_to_coast))

# --------  Plot  --------------------------------------------- #

sample_corr <- filter(corr2, in_water == 1)

sample_corr <- slice_sample(sample_corr, prop = .01, replace = FALSE) # randomly select 1% of observations

tmap_mode('view')

tm_shape(coast) + # Looks OK but did not verify using Measure tool in ArcGIS
  tm_lines(col = "green") + 
  tm_shape(corr) + 
  tm_symbols(col = popup.vars = 'dist_to_coast')

corr2$dist_to_coast <- as.numeric(corr2$dist_to_coast)

max(corr2$dist_to_coast, na.rm = TRUE) # 58 km
mean(corr2$dist_to_coast, na.rm = TRUE) # 11 km
sd(corr2$dist_to_coast, na.rm = TRUE) # 16 km 


