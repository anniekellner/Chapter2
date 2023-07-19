######################################################
#####   COMPARING MY DATA TO PAGANO ET AL. 2021 ######
######################################################

# Criteria for Pagano et al inclusion:
    # Data gap < 108 hr
    # Fix rate <= 4 hr

library(tidyverse)
library(sf)
library(terra)
library(conflicted)

rm(list = ls())

# ----------  LOAD AND PREP DATA   ----------------- #

pag <- read_csv('./Data/Pagano_bears.csv')
me <- readRDS('./Data/Derived-data/DFs/bears_ch2_052823.Rds')

me <- me %>%
  select(animal, year) %>%
  separate_wider_delim(animal, delim = "_", names = c(NA, "ID"), cols_remove = TRUE) %>%
  select(ID, year) %>%
  rename(YEAR = year) %>%
  distinct() %>%
  ungroup()

me <- me[,2:3]

me <- me %>%
  mutate(ID = as.double(ID))

dif <- setdiff(pag, me)

## ---------  GO BACK IN TIME TO SEE WHY I ELIMINATED THESE BEARS   ------------------- #

# From original clean_data script (first script in Ch2)
# load all_v2.Rds from 'old' folder in Data/Derived-data/DFs/Old

# All bears with ows data

b <- all_v2 %>%
  select(animal, year, month, land_bear_ows, land_bear) %>%
  filter(month > 6 & month < 11) %>%
  filter(land_bear_ows == 1) 

b1 <- b %>%
  group_by(animal, year) %>%
  slice_head() %>%
  separate_wider_delim(animal, delim = "_", names = c(NA, "ID"), cols_remove = TRUE) %>%
  select(ID, year) %>%
  mutate(ID = as.double(ID)) %>%
  rename(YEAR = year) 

setdiff(pag, b1) # all bears so far are included in pag

## I then removed bears with less than 100 data points

all <- all_v2 %>%
  separate_wider_delim(animal, delim = "_", names = c(NA, "ID"), cols_remove = TRUE) %>%
  mutate(ID = as.double(ID)) %>%
  rename(YEAR = year) 

b2 <- b1 %>%
  left_join(all) %>%
  select(ID, YEAR, gps_lat, gps_lon, datetime, X, Y, ymd)

b2 %>%
  group_by(ID) %>%
  add_count(ID) %>%
  filter(n < 100)

# Looks like all bears had over 100 pts

## Apply 7-day criteria

# load bears_092921.Rds, which was the df created after redefining landfall (df named during loading process)

b7days <- b7days %>%
  select(animal, year) %>%
  separate_wider_delim(animal, delim = "_", names = c(NA, "ID"), cols_remove = TRUE) %>%
  mutate(ID = as.double(ID)) %>%
  rename(YEAR = year) %>%
  distinct()

## THIS IS WHERE THE DISCREPANCY OCCURS ##

# ------- CHECK METHODS FOR EXCLUDING THESE BEARS   --------------------- #

b2sf <- st_as_sf(b2, coords = c('X','Y'), crs = 3338)
  
dem <- rast('./Data/Spatial/ans_dem_8bit.tif')

  

