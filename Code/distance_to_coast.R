########################################################
###   DISTANCE TO MAINLAND COAST   #####################
########################################################

## Only calculating distance to coast for mainland points (decided at meeting with GW 4/13/22)
## This metric is really only relevant for mainland points
## The positive/negative denotation messes with coefficients
# Used terra package because sf was not working well

# Coast = mainland coast. Digitized using combo of shapefile and IfSAR.

library(tidyverse)
library(sf)
library(terra)
library(tmap)
library(tmaptools)
library(here)


rm(list = ls())

source('./Code/MyFunctions.R')

# ----- Load data ---------------------------------- #

# Coastline

coast <- st_read('./Data/Spatial/coastline/digitized_coastline.shp') # Used .shp of coastline as digitized from IFSAR images on GEE
coast <- st_transform(coast, 3338)

coast_vec <- vect(coast) # convert coast to spatvector

# Used and available pts

uaSF <- readRDS(here("Data", "Derived-data", "DFs", "OG", "uaSF_12-13-23.Rds"))

uaSF_noNA <- uaSF %>%
  drop_na(elevation) # to reduce computational load, because these are water points anyway

uaVec <- vect(uaSF_noNA)



# --------  Calculate distance to coast  ------------------------- #

plot(uaVec)
plot(coast_vec, add = TRUE)

dist <- distance(uaVec, coast_vec, pairwise=FALSE, unit="m")

uaSF_noNA <- cbind(uaSF_noNA, dist)

# --------  Join with original df ------------------------------  #

# Drop geometries

uaSF <- st_drop_geometry(uaSF)
uaSF_noNA <- st_drop_geometry(uaSF_noNA) 

uaSF2 <- left_join(uaSF, uaSF_noNA)

uaSF[122,]

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









