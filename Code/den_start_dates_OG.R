#########################################################################
######    NEW END DATES FOR CHAPTER 2 ANALYSIS    #######################
#########################################################################

# Aug 10 2023 - add Pagano bears to analysis

# End dates:
  # Denning bears: when they enter dens 
  # Departing Bears: when they leave for ice
  # Other: When collars stop transmitting data

library(tidyverse)
library(sf)
library(here)
library(data.table)
library(conflicted)

conflicts_prefer(
  dplyr::filter(),
  lubridate::year()
)

rm(list = ls())


# ---- LOAD AND PREP DATA  ----------------- #

## Load

b <- readRDS(here("Data", "Derived-data", "DFs", "ch2_no_end_cutoff_080823.Rds")) 

oldCh2 <- readRDS(here('Data', 'Derived-data', 'DFs', 'bears_ch2_110622.Rds')) # to see if there are bears in new pag data that are denning bears

ch2IDs <- unique(b$id)

all <- readRDS(here("Data", "Derived-data", "DFs", "all_052323.Rds")) # has denning start dates

allUSGS <- read_csv(here("Data", "usgs_pbear_gps_ccde16_v20170131.csv")) # to get all data for denning bears

den <- read_csv(here('Data', 'Denning_locs_dates.csv')) %>%
  rename(animal = ...1) 

## Prep 

# USGS 

allUSGS <- allUSGS %>% # add columns for id, date, time
  unite("id", c("animal", "year"), sep = '.', remove = FALSE) %>% # add column for id
  unite("date", year:day, sep = '-', remove = FALSE) %>% # do not remove original columns
  unite("time", hour:second, sep = ':', remove = FALSE) 

allUSGS <- allUSGS %>% # add datetime
  unite("datetime", c("date", "time"), sep = " ", remove = FALSE) %>% glimpse()

allUSGS$datetime <- ymd_hms(allUSGS$datetime, tz = "US/Alaska")
allUSGS$date <- ymd(allUSGS$date)

# Denning data 

den$entrance_ymd <- mdy(den$entrance)
den$year <- year(as.character(den$entrance_ymd)) 

denIDs <- unique(den$id)

# -- ADD STUDY END DATES FOR DENNING BEARS  --------------- #

allDen <- all %>% # Just to get IDs - later will get all data
  filter(enter_den == 1 | 
           id %in% denIDs & 
           id %in% ch2IDs) 

allDenIDs <- unique(allDen$id)

# Get all dates from 'all' df for all denning bears

allDen <- all %>%
  filter(id %in% allDenIDs & month > 9) 


#   --------- GET DENNING LOCATIONS ----------------------------------  #

# Mode method does not work for all bears. Use last location of the year. 

denLocs <- allDen %>%
  group_by(id) %>%
  select(id, gps_lat, gps_lon) %>%
  slice_tail()

denLocs$gps_lat <- round(denLocs$gps_lat, 1)
denLocs$gps_lon <- round(denLocs$gps_lon, 1)

#saveRDS(denLocs, file = here("Data", "Derived-data", "DFs", "Den_Locations.Rds")) # saved with full gps locs, not rounded

# Merge with all USGS data

ch2Fall <- allUSGS %>%
  select(id:rate) %>%
  filter(month > 9)
  
ch2Fall <- filter(ch2Fall, id %in% ch2IDs) 
ch2Fall$gps_lat <- round(ch2Fall$gps_lat, 1) # round to match denLocs and avoid GPS error
ch2Fall$gps_lon <- round(ch2Fall$gps_lon, 1)

inDen <- denLocs %>%
  inner_join(ch2Fall)

inDen$at_densite <- 1

# Merge in_den back into ch2Fall to get all dates

ch2Fall <- ch2Fall %>%
  left_join(inDen) %>%
  replace_na(list(at_densite = 0))

# ----------  3-DAY CRITERIA  -------------------- #

# Daily data

ch2Fall<- ch2Fall %>% # if bear is in den at all that day, in_den = 1
  group_by(id, date) %>%
  mutate(den_day = any(at_densite == 1)) %>%
  ungroup()

ch2Fall <- ch2Fall %>% # add column for denning_bear so is easily retrievable
  group_by(id) %>%
  mutate(denning_bear = if_else(any(at_densite == 1), 1, 0)) %>% 
  ungroup()

# Select only first entry of the day - denDaily DF (this avoids differences between GPS fix intervals)

denDaily <- ch2Fall %>%
  filter(denning_bear == 1) %>%
  group_by(id, date) %>%
  slice_head() %>%
  ungroup()

denDaily<- denDaily %>%
  mutate(den_day = if_else(den_day == TRUE, 1, 0)) %>%
  group_by(id) %>%
  mutate(rowNum = row_number()) %>% 
  select(id, date, gps_lat, gps_lon, den_day:rowNum) %>%
  ungroup()

# Use data.table to get cumulative den time with reset

setDT(denDaily)

denDaily[, cumDen := den_day*cumsum(den_day), .(id, rleid(den_day))] ## LOOKS GOOD!!!!!!

# --- GET ACTUAL DATETIME OF DEN ENTRANCE ----------- #

# Create dataframe of day 1's by starting with Day 3 and going back 2 rows. 
# No bears visited den for more than 3 days prior to entrance (upon inspection of data)

rowNum_day1 <- denDaily %>%
  filter(cumDen == 3) %>%
  mutate(day1row = rowNum - 2) %>%
  select(id, day1row) 

rowNum_day1 <- rowNum_day1 %>%
  rename(rowNum = day1row)
  
day1date <- semi_join(denDaily, rowNum_day1)
day1date <- select(day1date, id, date)

# Pull all points from specified date and add a column for enter_den

entranceDate <- day1date %>%
  left_join(ch2Fall) %>%
  group_by(id) %>%
  filter(at_densite == 1) %>%
  select(id:datetime, gps_lon, gps_lat, distance:denning_bear) %>%
  slice_head()

entranceDate$enter_den <- 1

# -----   JOIN BACK INTO MAIN DATAFRAMFE  --------- #

# Join back into ch2Fall df with additional enter_den column (will also be start_date)
# Ch2 does not have enough dates - did not add dates after 11-1 when creating df

ch2Fall2 <- ch2Fall %>%
  left_join(entranceDate)

filter(ch2Fall2, enter_den == 1) # looks good

which(is.na(ch2Fall2$gps_lat)) # no missing lat/lon values

# ---- MERGE LAT/LON SO DF HAS ALL VALUES ----------- #

# Create owsUSGS for complete lat/lon list

owsUSGS <- allUSGS %>%
  filter(month > 6 & id %in% ch2IDs) %>%
  select(id:rate)

ch2Fall2 <- ch2Fall2 %>%
  select(id, date, datetime, distance, rate, at_densite, denning_bear, enter_den)

owsUSGS <- owsUSGS %>%
  full_join(ch2Fall2) %>%
  rename(ymd = date)

filter(owsUSGS, enter_den == 1)

# merge with previous ch2 data

b <- b %>% # remove lat/lon from b so gps coords come from owsUSGS
  select(id, ymd, datetime, departure_to_ice, land, landfall, age, repro, at_bonepile, start.swim, collar_drop, study_end, ordinal_date)

b2 <- b %>% 
  full_join(owsUSGS) 
  
# Checks 

filter(b2, enter_den == 1) # check enter_den date
unique(b2$id) # check ID's

## NEED TO CHECK PAGANO ID'S AND ELIMINATE ANY THAT DO NOT QUALIFY

setDT(b2)
anyDuplicated(b2) # check for duplicate rows

b %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  slice_head()

b %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  slice_tail() %>%
  ungroup()



