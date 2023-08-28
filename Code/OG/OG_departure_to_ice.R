##########################################################################
###     RE-RUN DATES OF DEPARTURE TO ICE    ##############################
##########################################################################

# Aug 26, 2023
# OG bears

library(tidyverse)
library(sf)
library(terra)
library(here)
library(tmap)
library(tmaptools)
library(conflicted)

conflicts_prefer(
  dplyr::filter(),
  terra::extract()
)

rm(list = ls())

# ----  LOAD AND PREP CH2 DATA ----------------- #

## Load

b <- readRDS(here("Data", "Derived-data", "DFs", "OG_ch2_082623.Rds")) # Bears

demPoly_5k <- st_read(here("Data", "Spatial", "DEM", "AK_CA_5kbuff", "AK_CA_5kbuff.shp")) # Spatial

rec <- st_read(here("Data", "Spatial", "Rectangle", "Rectangle.shp")) # Spatial

## Prep

rec <- st_transform(rec, st_crs(demPoly_5k))

demPoly_5k_crop <- st_crop(demPoly_5k, rec)

buff5k <- demPoly_5k_crop %>%
  filter(OBJECTID == 14) # excludes Canada

b <- select(b, -land) # not correct: did not account for all bears and all dates

bsf <- st_as_sf(b, coords = c("gps_lon", "gps_lat"), crs = 4326)
bsf <- st_transform(bsf, st_crs(demPoly_5k))

# Plot check - looks good- consider eliminating points east of AK-Canada border

#tm_shape(demPoly_5k_crop) + 
  #tm_polygons() + 
  #tm_shape(bsf) + 
  #tm_symbols(col = "red")

# ------- 7-DAY CRITERIA  ---------------- #

#bsf <- select(bsf, -land) # land column is not correct 

bsf$land <- st_intersects(bsf, buff5k) %>% lengths > 0 # https://stackoverflow.com/questions/49294933/r-convert-output-from-sfst-within-to-vector

landDaily <- bsf %>% # Create column for on_land = TRUE or FALSE (for later, if bear was on land that day at any point)
  group_by(id, ymd) %>%
  mutate(on_land = any(land == TRUE)) %>%
  ungroup()

on_ice <- landDaily %>% # Create column for on_ice (land = FALSE)
  mutate(on_ice = if_else(
  on_land == TRUE, 0, 1)) 

on_ice <- on_ice %>% # Take the first daily observation in order to see whether bear used land that day
  group_by(id, ymd) %>%
  slice_head() %>%
  ungroup()

on_ice <- on_ice %>%
  group_by(id) %>%
  mutate(cum_ice = cumsum(on_ice)) %>%
  mutate(rowNum = row_number()) %>% glimpse()

day7 <- on_ice %>% 
  filter(cum_ice == 7) %>% 
  select(id, ymd, rowNum) 
