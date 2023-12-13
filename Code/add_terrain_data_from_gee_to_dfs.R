###############################
##    EXTRACT TERRAIN DATA  ###
###############################

# Data from shapefiles created using Google Earth Engine

library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library(here)
library(conflicted)

rm(list = ls())

# --------------------   LOAD DATA   ----------------------- #

# GEE SHP

terr <- st_read(here("Data", "Derived-data", "Spatial", "Terrain", "Terrain_GEE_12_12_23", "terrain_used_avail.shp")) 

# Plot to see that values look right

tmap_mode('view')

tm_shape(terr) + 
  tm_symbols()

# R DF

ua <- readRDS(here("Data", "Derived-data", "DFs", "OG", "allUA.Rds"))

#   --------------    ADJUST VALUES   --------------  #

# Terrain - classify as categorical

terr <- terr %>%
  mutate(aspect2 = case_when(
    aspect >= 315 | aspect <= 45 ~ "North",
    aspect > 45 | aspect < 135 ~ "East",
    aspect > 135 | aspect < 225 ~ "South",
    aspect > 225 | aspect < 315 ~ "West"
  )) %>%
  replace_na(list(aspect2 = "Flat")) %>%
  select(-aspect) %>%
  rename(aspect = aspect2) %>% glimpse()

# Record lat/lon values

terr2 <- terr %>%
  bind_cols(st_coordinates(terr)) %>%
  rename(gps_lon = X) %>%
  rename(gps_lat = Y)

# Record AA values

terr2 <- st_transform(terr2, 3338) # project to AA

terr2 <- terr2 %>%
  bind_cols(st_coordinates(terr2)) %>%
  rename(Xaa = X) %>%
  rename(Yaa = Y)

#saveRDS(terr2, here("Data", "Derived-data", "DFs", "OG", "uaSF_091823.Rds"))






# Remove previous columns for elevation, aspect and slope

corr <- select(corr, -c(elevation:slope))
bone <- select(bone, -c(elevation:slope))

# --- Join --------------------------- #

# Convert into dataframes

corr.gee <- as.data.frame(corr.gee)
bone.gee <- as.data.frame(bone.gee)

corr <- as.data.frame(corr)
bone <- as.data.frame(bone)

# Get rid of extraneous columns

corr.gee <- select(corr.gee, elevation, aspect, Point_ID, slope)
bone.gee <- dplyr::select(bone.gee, elevation, aspect, Point_ID, slope)

# Join by Point_ID

corr2 <- left_join(corr, corr.gee)
bone2 <- left_join(bone, bone.gee)

# Make NA values 0 for elevation (because are typically deep sea points)

corr2$elevation <- replace_na(0)
bone2$elevation <- replace_na(0)

# Save

saveRDS(corr2, './Data/Derived-data/corridor_data.Rds')
saveRDS(bone2, './Data/Derived-data/bonepile_data.Rds')
