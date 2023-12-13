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

samp <- slice_sample(terr, prop = 0.1) # too many points, so sampled 10%

# Plot to see that values look right

tmap_mode('view')

tm_shape(samp) + # look good
  tm_symbols()

# R DF

ua <- readRDS(here("Data", "Derived-data", "DFs", "OG", "uaSF.Rds"))

#   --------------    ADJUST VALUES   --------------  #

# Terrain - classify as categorical

terr <- terr %>%
  mutate(aspect_cat = case_when(
    aspect >= 315 | aspect <= 45 ~ "North",
    aspect > 45 | aspect < 135 ~ "East",
    aspect > 135 | aspect < 225 ~ "South",
    aspect > 225 | aspect < 315 ~ "West"
  )) %>%
  replace_na(list(aspect_cat = "Flat")) 
  #select(-aspect) %>%
  #rename(aspect = aspect2) %>% glimpse()

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

terr2 <- select(terr2, -hillshade)

#saveRDS(terr2, here("Data", "Derived-data", "DFs", "OG", "uaSF_12-12-23.Rds"))




