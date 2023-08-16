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

source(here('Code', 'MyFunctions.R')) # for Mode fxn

# ---- LOAD AND PREP DATA  ----------------- #

b <- readRDS(here("Data", "Derived-data", "DFs", "ch2_no_end_cutoff_080823.Rds")) 

ch2IDs <- unique(b$id)

all <- readRDS(here("Data", "Derived-data", "DFs", "all_052323.Rds")) # has denning start dates

allUSGS <- read_csv(here("Data", "usgs_pbear_gps_ccde16_v20170131.csv")) # to get all data for denning bears

den <- read_csv(here('Data', 'Denning_locs_dates.csv')) %>%
  rename(animal = ...1) 

den$entrance_ymd <- mdy(den$entrance)
den$year <- year(as.character(den$entrance_ymd)) 

den <- den %>% 
  unite("id", c("animal", "year"), sep = '.')

denIDs <- unique(den$id)

oldCh2 <- readRDS(here('Data', 'Derived-data', 'DFs', 'bears_ch2_110622.Rds')) # to see if there are bears in new pag data that are denning bears


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

#saveRDS(denLocs, file = here("Data", "Derived-data", "DFs", "Den_Locations.Rds"))

# Merge with all USGS data

all_fall <- allUSGS %>%
  select(animal:rate) %>%
  filter(month > 9) %>%
  unite("id", c("animal", "year"), sep = '.', remove = FALSE) 

all_fall$gps_lat <- round(all_fall$gps_lat, 1) # round to match denLocs and avoid GPS error
all_fall$gps_lon <- round(all_fall$gps_lon, 1)

inDen <- denLocs %>%
  inner_join(all_fall)

inDen$at_densite <- 1

# Merge in_den back into all_fall to get all dates

all_fall <- all_fall %>%
  left_join(inDen) %>%
  replace_na(list(at_densite = 0))

# Criteria that bear needs to stay in den for > 5 days 

# Prep USGS data (now all_fall) by adding datetime

all_fall <- all_fall %>%
  unite("date", year:day, sep = '-', remove = FALSE) %>% # do not remove original columns
  unite("time", hour:second, sep = ':', remove = FALSE) 

all_fall <- all_fall %>%
  unite("datetime", c("date", "time"), sep = " ", remove = FALSE) %>% glimpse()
  
all_fall$datetime <- ymd_hms(all_fall$datetime, tz = "US/Alaska")
all_fall$date <- ymd(all_fall$date)

# ------  DAILY DATA  ---------------- #

all_fall<- all_fall %>% # if bear is in den at all that day, in_den = 1
  group_by(id, date) %>%
  mutate(in_den = any(at_densite == 1)) %>%
  ungroup()

all_fall <- all_fall %>% # add column for denning_bear so is easily retrievable
  group_by(id) %>%
  mutate(denning_bear = if_else(any(at_densite == 1), 1, 0)) %>% glimpse()

# Select only first entry of the day - denDaily DF (this avoids differences between GPS fix intervals)

denDaily <- all_fall %>%
  filter(denning_bear == 1) %>%
  group_by(id, date) %>%
  slice_head() %>%
  ungroup()

denDaily<- denDaily %>%
  mutate(in_den = if_else(in_den == TRUE, 1, 0)) %>%
  group_by(id) %>%
  mutate(rowNum = row_number()) %>% 
  select(id, date, gps_lat, gps_lon, in_den:rowNum) %>%
  ungroup()

# Use data.table to get cumulative den time with reset

setDT(denDaily)

denDaily[, cumDen := in_den*cumsum(in_den), .(id, rleid(in_den))] ## LOOKS GOOD!!!!!!

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

enterDen <- day1date %>%
  left_join(all_fall) %>%
  group_by(id) %>%
  filter(at_densite == 1) %>%
  slice_head()

enterDen$enter_den <- 1

# Join back into ch2 df with enter_den column (will also be start_date)

enterDen <- enterDen %>%
  rename(ymd = date)

test <- all %>%
  full_join(enterDen)


b %>% group_by(id) %>% slice_tail()






