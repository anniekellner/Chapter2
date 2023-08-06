############################################################################
#########   RE-RUN CHAPTER 2 BEARS WITH NEW LANDFALL SCRIPT   ##############
############################################################################

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

all <- readRDS("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/Chapter2/Data/Derived-data/DFs/all_052323.Rds")

ch2 <- readRDS("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/Chapter2/Data/Derived-data/DFs/bears_ch2_052823.Rds")

iceIDs <- ch2 %>% # exclude bears collared on land
  filter(landfall == 1) 

iceIDs <- unique(iceIDs$id)

allCh2 <- all %>%
  filter(id %in% iceIDs) %>%
  filter(month > 6 & month < 11) %>%
  select(-land)

# Spatial

demPoly_5k <- st_read('./Data/Spatial/DEM/AK_CA_5kbuff/AK_CA_5kbuff.shp')

rec <- st_read('./Data/Spatial/Rectangle/rectangle.shp')
rec <- st_transform(rec, st_crs(demPoly_5k))

demPoly_5k_crop <- st_crop(demPoly_5k, rec)

buff5k <- demPoly_5k_crop %>%
  filter(OBJECTID == 14) # excludes Canada

# -------   RE-DO 7-DAY CRITERIA ---------------- #

ch2sf <- st_as_sf(allCh2, coords = c('gps_lon', 'gps_lat'), crs = 4326)
ch2sf <- st_transform(ch2sf, st_crs(buff5k))

ch2sf$land <- st_intersects(ch2sf, buff5k) %>% lengths > 0 # https://stackoverflow.com/questions/49294933/r-convert-output-from-sfst-within-to-vector


## 7-day criteria

lb <- ch2sf %>% # Create column for Land = TRUE or FALSE
  group_by(id, ymd) %>%
  mutate(on_land = any(land == TRUE)) %>%
  ungroup()

lb <- lb %>% # switch to binary 0/1
  mutate(on_land = if_else(
    on_land == TRUE, 1, 0)) 

lb <- lb %>% # Take the first daily observation in order to see whether bear used land that day
  group_by(id, ymd) %>%
  slice_head() %>%
  ungroup()

lb <- lb %>%
  group_by(id) %>%
  mutate(cum_land = cumsum(on_land)) %>%
  mutate(rowNum = row_number()) %>% glimpse()

day7 <- lb %>% 
  filter(cum_land == 7) %>% 
  select(id, ymd, rowNum) 

day7 <- day7 %>%
  group_by(id) %>%
  arrange(id, ymd) %>%
  slice_head()

row_number_day1 <- day7 %>% # get row number from day 7 that's affiliated with the dates 
  mutate(Day1row = rowNum - 6) 

day1 <- lb %>%
  filter(id == "pb_20333.2008" & rowNum == 56 |
           id == "pb_20414.2009" & rowNum == 23 |
           id == "pb_20418.2005" & rowNum == 56 | 
           id == "pb_20520.2012" & rowNum == 44 | 
           id == "pb_20525.2013" & rowNum == 49 |
           id == "pb_20525.2014" & rowNum == 39 |
           id == "pb_20735.2009" & rowNum == 27 | 
           id == "pb_20845.2015" & rowNum == 56 | 
           id == "pb_21015.2013" & rowNum == 48 | 
           id == "pb_21237.2011" & rowNum == 9 | 
           id == "pb_21368.2014" & rowNum == 42 | 
           id == "pb_32366.2011" & rowNum == 36 | 
           id == "pb_32366.2014" & rowNum == 42) %>%
  select(id, ymd) %>%
  st_drop_geometry()

ch2sf <- ch2sf %>%
  st_drop_geometry() %>%
  select(id, ymd, datetime, land)

landfall <- ch2sf %>%
  left_join(day1, by = c("id", "ymd")) %>% 
  group_by(id) %>%
  arrange(id, datetime) %>%
  filter(land == TRUE) %>%
  slice_head() 

landfall$landfall <- 1

# ----- MERGE INTO LARGER DATAFRAME -------------- #

allCh2 <- allCh2 %>%
  select(id, animal:datetime, X, Y, ymd, departure_to_ice, den_location, enter_den) %>%
  full_join(landfall) %>% glimpse()

test <- allCh2 %>% filter(landfall == 1)

saveRDS(allCh2, file = './Data/Derived-data/DFs/bears_ch2_080523.Rds')
  



