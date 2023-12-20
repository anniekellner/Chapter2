###########################################
##    ADD TERRAIN DATA TO USED-AVAIL DF ###
###########################################

# Points outside ifSAR have NA values for all terrain variables

### NEED TO ADD TERRAIN DATA TO FULL DATAFRAME (cannot just substitute GEE shapefile because not all pts represented)

# Data from shapefiles created using Google Earth Engine

library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library(here)
library(conflicted)

rm(list = ls())

# --------------------   LOAD AND PREP DATA   ----------------------- #

# GEE SHP

terr <- st_read(here("Data", "Derived-data", "Spatial", "Terrain", "Terrain_GEE_12_20_23", "terrain_used_avail_12_20_23.shp")) 

samp <- slice_sample(terr, prop = 0.01) # too many points, so sampled 1%

# Plot to see that values look right

tmap_mode('view')

tm_shape(samp) + # look good
  tm_symbols()

# Remove unnecessary variables

terr <- terr %>%
  select(-c("hillshade", "t2_", "sl_", "step_id_", "Xaa", "Yaa"))

# R DF

ua <- readRDS(here("Data", "Derived-Data", "DFs", "OG", "uaSF_12-20-23.Rds"))
ua <- st_drop_geometry(ua)

ua <- ua %>%
  mutate(on_island = ifelse(on_island == TRUE, 1, 0))

#   --------------    ADJUST VALUES   --------------  #

# Terrain - classify as categorical

terr <- terr %>% 
  mutate(aspect_cat = case_when(
    aspect >= 315 | aspect <= 45 ~ "North",
    aspect > 45 | aspect < 135 ~ "East",
    aspect > 135 | aspect < 225 ~ "South",
    aspect > 225 | aspect < 315 ~ "West"
  )) %>%
  replace_na(list(aspect_cat = "Flat")) ### WATER POINTS WILL HAVE NA VALUES SO BE SURE TO ONLY INCLUDE LAND PTS DURING ANALYSIS
  #select(-aspect) %>%
  #rename(aspect = aspect2) %>% glimpse()

# Record lat/lon values

terr2 <- terr %>%
  bind_cols(st_coordinates(terr)) %>%
  rename(gps_lon_GEE = X) %>%
  rename(gps_lat_GEE = Y)

# Record AA values

terr2 <- st_transform(terr2, 3338) # project to AA

terr2 <- terr2 %>%
  bind_cols(st_coordinates(terr2)) %>%
  rename(Xaa_GEE = X) %>%
  rename(Yaa_GEE = Y)

terr2 <- terr2 %>%
  st_drop_geometry()

# ----------  BIND DATASETS --------------- #

ua2 <- left_join(ua, terr2, by = c("id", "analysis", "used", "on_island", "row"))

ua2 <- ua2 %>%
  select(-"ta_.y") %>%
  rename(turning_angle = ta_.x)

#saveRDS(ua2, here("Data", "Derived-data", "DFs", "OG", "uaSF_12-20-23.Rds"))




