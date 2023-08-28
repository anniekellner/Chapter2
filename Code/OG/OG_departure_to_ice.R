##########################################################################
###     RE-RUN DATES OF DEPARTURE TO ICE    ##############################
##########################################################################

## CRITERIA: Departure to ice = the first day between October and December after which bear does not return to land for 7 days

# Aug 26, 2023
# OG bears

library(tidyverse)
library(sf)
library(terra)
library(here)
library(tmap)
library(tmaptools)
library(data.table)
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

b <- select(b, -c(land, departure_to_ice)) # not correct: did not account for all bears and all dates

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

on_ice <- bsf %>% # Create column for on_ice (land = FALSE); if bear was on ice that day = 1, otherwise 0
  group_by(id, ymd) %>%
  mutate(on_ice = if_else(any(land == TRUE), 0, 1)) %>%
  ungroup() %>% glimpse()
    
iceDaily <- on_ice %>% # Take the first daily observation in order to see whether bear used land that day
  group_by(id, ymd) %>%
  slice_head() %>%
  ungroup() %>% glimpse()

iceDaily <- iceDaily %>% 
  group_by(id) %>%
  filter(month > 9) %>% # Do not count water excursions prior to October
  mutate(rowNum = row_number()) %>% 
  select(id, ymd, datetime, on_ice, rowNum) %>% glimpse()

# Use data.table to get cumulative den time with reset

setDT(iceDaily)

iceDaily[, days_on_ice := on_ice*cumsum(on_ice), .(id, rleid(on_ice))] 

# --- GET DEPARTURE DATETIME  --------- #

lastLandDate <- iceDaily %>% 
  filter(days_on_ice == 7) %>% 
  group_by(id) %>% # Because some bears leave ice and then return, but the return isn't biologically meaningful (based on data examination)
  slice_head() %>%
  mutate(departRow = rowNum - 7) %>% # Because need the day PRIOR to the first day bear is not on land
  group_by(id) %>%
  slice_head() %>%
  select(id, departRow)

lastLandDate <- lastLandDate %>%
  rename(rowNum = departRow)

departDate <- semi_join(iceDaily, lastLandDate)
departDate <- select(departDate, id, ymd)

# Match with dates from main df 

departDatetime <- departDate %>%
  left_join(bsf) %>%
  group_by(id) %>%
  filter(land == TRUE) %>%
  slice_tail() %>%
  ungroup()

departDatetime$departure_to_ice <- 1

departDatetime <- st_drop_geometry(departDatetime)

# ------  JOIN BACK INTO MAIN DF ----------- #

# First join back into bsf to get 'land' column

bsf2 <- bsf %>%
  left_join(departDatetime)

filter(bsf2, departure_to_ice == 1) # row numbers identical - no duplicates
  
bsf2 <- bsf2 %>%
  st_drop_geometry() %>%
  replace_na(list(departure_to_ice = 0))

b2 <- b %>%
  left_join(bsf2)

filter(b2, departure_to_ice == 1) # row numbers identical - no duplicates

#saveRDS(b2, here("Data", "Derived-data", "DFs", "OG", "OG_add_depart_ice.Rds"))
