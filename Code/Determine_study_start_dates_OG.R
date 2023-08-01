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
  filter(id == "pb_20446.2009" | # included
           id == "pb_20529.2005" | # included
           id == "pb_21264.2011" | # included
           id == "pb_21358.2013" | # included
           id == "pb_20529.2004") %>%
  filter(month > 6 & month < 11)

# Spatial

demPoly_5k <- st_read('./Data/Spatial/DEM/AK_CA_5kbuff/AK_CA_5kbuff.shp')

rec <- st_read('./Data/Spatial/Rectangle/rectangle.shp')
rec <- st_transform(rec, st_crs(demPoly_5k))

demPoly_5k_crop <- st_crop(demPoly_5k, rec)

buff5k <- demPoly_5k_crop %>%
  filter(OBJECTID == 14) # excludes Canada

# -------   BEARS THAT ARRIVED FROM ICE ---------------- #

iceSF <- st_as_sf(ice, coords = c('gps_lon', 'gps_lat'), crs = 4326)
iceSF <- st_transform(iceSF, st_crs(buff5k))

iceSF$Land <- st_intersects(iceSF, buff5k) %>% lengths > 0 # https://stackoverflow.com/questions/49294933/r-convert-output-from-sfst-within-to-vector

iceSF <- iceSF %>% # remove previous column for land
  select(-land) 


## 7-day criteria

lb <- iceSF %>% 
  group_by(id, ymd) %>%
  mutate(on_land = any(Land == TRUE)) %>%
  ungroup()

lb <- lb %>%
  mutate(on_land = if_else(
    on_land == TRUE, 1, 0)) 

lb <- lb %>%
  group_by(id, ymd) %>%
  slice_head() %>%
  ungroup()

lb <- lb %>%
  group_by(id) %>%
  mutate(cum_land = cumsum(on_land)) 

day7 <- lb %>% filter(cum_land == 7) %>% select(id, ymd)

day1 <- day7 %>%
  mutate(Day1 = ymd - ddays(7)) %>%
  select(id, geometry, Day1) %>%
  rename(ymd = Day1) %>%
  st_drop_geometry()

ice2 <- iceSF %>%
  st_drop_geometry()

landfall <- day1 %>%
  left_join(ice2, by = c("id", "ymd"))

landfall <- landfall %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  filter(Land == TRUE) %>%
  slice_head()
  
 



