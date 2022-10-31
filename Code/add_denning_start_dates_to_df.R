################################################################
###   ADD DENNING START DATES TO DATAFRAME  ####################
################################################################

# id <- 'pb_XXXXX.XX' does not work for some reason

library(dplyr)
library(sf)

rm(list = ls())

source('./Code/MyFunctions.R')

# --- LOAD DATA ------------- #

all <- readRDS('./Data/Derived-data/DFs/all_v2.Rds') # all data (df)

# --- PREP DATA ------------------- #

x <- all %>% 
  filter(id == 'pb_21237.2011' & month > 9) %>%
  st_as_sf(coords = c('gps_lat', 'gps_lon'), crs = 4326, remove = FALSE) # do not remove gps_lat and lon from df

denLat <- Mode(x$gps_lat) 
denLon <- Mode(x$gps_lon) 

# Make sf

denLoc <- st_point(x = c(denLat, denLon))
geom <- st_geometry(denLoc)

# ----  JOIN TO DATAFRAME ---------------- #

x$den_location <- ifelse(x$geometry == geom, 1, 0)

x2 <- select(x, id, den_location)

all2 <- all %>%
  left_join(x2)  
