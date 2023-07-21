####################################################################
########    DETERMINE STUDY START DATES   ##########################  
########       INCLUDE PAGANO DATA           #######################
####################################################################

# see ice_arrive_depart.csv for info from Chapter 1
# No bears from 2009 in this study were collared on land


library(tidyverse)
library(sf)
library(terra)
#library(leaflet)
library(zoo)
library(conflicted)

conflicts_prefer(
  dplyr::filter(),
  terra::extract()
)

rm(list = ls())

# --------    LOAD AND PREP DATA   --------------------  #

# Bears

missing <- readRDS("Data/Derived-data/DFs/missing_bears.Rds")

missing %>%
  filter(end.swim == 1) # 20446.2009, 20529.2005, 21264.2011, 21358.2013, 20529.2004

ice <- missing %>%
  filter(id == "pb_20446.2009" | 
           id == "pb_20529.2005" | 
           id == "pb_21264.2011" | 
           id == "pb_21358.2013" | 
           id == "20529.2004")

# Spatial

demPoly_5k <- st_read('./Data/Spatial/DEM/AK_CA_5kbuff/AK_CA_5kbuff.shp')

rec <- st_read('./Data/Spatial/Rectangle/rectangle.shp')
rec <- st_transform(rec, st_crs(demPoly_5k))

demPoly_5k_crop <- st_crop(demPoly_5k, rec)

buff5k <- demPoly_5k_crop %>%
  filter(OBJECTID == 14) # excludes Canada

# -------   BEARS THAT ARRIVED FROM ICE ---------------- #





# Spatial data

b2sf <- st_transform(b2sf, st_crs(buff5k))

test <- b2sf %>%
  mutate(Land = if_else(st_intersects(., buff5k) == TRUE, 1, 0))

b2sf$Land <- st_intersects(b2sf, buff5k) %>% lengths > 0 # https://stackoverflow.com/questions/49294933/r-convert-output-from-sfst-within-to-vector

## 7-day criteria

lb <- b2sf %>% 
  group_by(ID, ymd) %>%
  mutate(on_land = any(Land == TRUE)) %>%
  mutate(consec_seven = rollapply(on_land, 7, all, align = 'left', fill = NA)) %>%
  filter(consec_seven == TRUE)


