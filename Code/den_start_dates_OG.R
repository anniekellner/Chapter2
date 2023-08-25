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

pag <- read_csv(here("Data", "Pagano_bears.csv"))


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

allUSGS <- allUSGS %>%
  select(id:rate) 

# Denning data 

den$entrance_ymd <- mdy(den$entrance)
den$year <- year(as.character(den$entrance_ymd)) 
den <- den %>%
  unite("id", animal, year, sep = '.', remove = FALSE)

denIDs <- unique(den$id)

# Pagano bears

pag <- pag %>%
  select(`Bear ID`, Year) %>%
  mutate(`Bear ID` = as.character(`Bear ID`)) %>%
  unite("id", `Bear ID`:Year, sep = '.', remove = TRUE) %>%
  mutate(id = paste0("pb_", id))

pag[1,] <- "pb_06810.2008"

# -- ADD STUDY END DATES FOR DENNING BEARS  --------------- #

# all df is all USGS data with added columns

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

USGS_fall <- allUSGS %>% # Need to exclude spring dates because bears are still in dens in spring
  select(id:rate) %>%
  filter(month > 9)
  
# Round GPS data for ch2Fall df
gpsRound <- filter(USGS_fall, id %in% ch2IDs) 
gpsRound$gps_lat <- round(gpsRound$gps_lat, 1) # round to match denLocs and avoid GPS error
gpsRound$gps_lon <- round(gpsRound$gps_lon, 1)

inDen <- denLocs %>%
  inner_join(gpsRound)

inDen$at_densite <- 1

# Merge in_den back into gpsRound (all USGS > Sept) to get all dates

noGPS <- gpsRound %>%
  left_join(inDen) %>%
  replace_na(list(at_densite = 0)) %>%
  select(-c(gps_lat, gps_lon)) # in order to merge back by datetime and recover full gps locations

# noGPS = all fall USGS data with at_densite added

# Merge into allUSGS df

ch2USGS_ows <- allUSGS %>%
  filter(id %in% ch2IDs & month > 6) %>% # ows only
  left_join(noGPS) %>%
  replace_na(list(at_densite = 0)) %>% glimpse()

# ----------  3-DAY CRITERIA  -------------------- #

# Daily data

ch2USGS_ows<- ch2USGS_ows %>% # if bear is in den at all that day, in_den = 1
  group_by(id, date) %>%
  mutate(den_day = any(at_densite == 1)) %>%
  ungroup() %>% glimpse()

ch2USGS_ows <- ch2USGS_ows %>% # add column for denning_bear so is easily retrievable
  group_by(id) %>%
  mutate(denning_bear = if_else(any(at_densite == 1), 1, 0)) %>% 
  ungroup() %>% glimpse()

# Select only first entry of the day - denDaily DF (this avoids differences between GPS fix intervals)

denDaily <- ch2USGS_ows %>%
  filter(denning_bear == 1) %>%
  group_by(id, date) %>%
  slice_head() %>%
  ungroup()

denDaily<- denDaily %>%
  mutate(den_day = if_else(den_day == TRUE, 1, 0)) %>%
  group_by(id) %>%
  mutate(rowNum = row_number()) %>% 
  select(id, date, gps_lat, gps_lon, den_day:rowNum) %>%
  ungroup() %>% glimpse()

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
  left_join(ch2USGS_ows) %>%
  group_by(id) %>%
  filter(at_densite == 1) %>%
  select(id:datetime, gps_lon, gps_lat, distance:denning_bear) %>%
  slice_head()

entranceDate$enter_den <- 1

# -----   JOIN BACK INTO USGS DATAFRAMFE  --------- #

# Join back into ch2USGS_ows with additional denning-related columns

ch2USGS_ows <- ch2USGS_ows %>% # starts at July 1
  left_join(entranceDate) %>% 
  rename(ymd = date) %>%
  select(-time) %>%
  replace_na(list(enter_den = 0)) %>%
  glimpse()

ch2USGS_ows %>% group_by(id) %>% slice_head()
b %>% group_by(id) %>% slice_head() %>% print(n = 28) # none start prior to July 1

filter(ch2USGS_ows, enter_den == 1) # looks good


# ----- MERGE WITH EXISTING CHAPTER 2 DATA  ----------------------- #

# Make separate df for categorical variables

bCat <- b %>%
  select(id, age, repro) %>%
  group_by(id) %>%
  slice_head() %>% print(n = length(unique(b$id)))

addBack <- b %>% # add back with left_join
  select(id:rate, departure_to_ice:collar_drop)

b <- b %>% # remove columns that will need to be changed 
  select(id:ymd)

# b (previous chapter 2 data) and USGS data should not have duplicate rows (theoretically) since all common variables can be matched

ch2 <-ch2USGS_ows %>% 
  left_join(b) 

# Checks 

filter(ch2, enter_den == 1) # check enter_den date
unique(ch2$id) # check ID's

## --- CHECK AGAINST PAGANO BEARS AND ELIMINATE BEARS THAT ARE NOT INCLUDED IN HIS LIST --------- #

# Criteria for pag bears: no gap in data > 108 hrs and fix rate <= 4 hrs

pagIDs <- pag$id
myIDs <- unique(ch2$id)

dif <- setdiff(myIDs, pagIDs) # "pb_20418.2005" "pb_21237.2011" "pb_32255.2008"
dif

# ----------  CHECK FOR DUPLICATES ------------ #

# Check for duplicate id/datetime combos

dupCheck <- ch2 %>%
  select(id, datetime)

setDT(dupCheck) 
anyDuplicated(dupCheck) # looks good!

# ------  REMOVE DATES PRIOR TO STUDY_START --------------- #

# Not all start dates are included. Need to cross-reference with original df

ch2 <- ch2 %>% select(-study_start)

b %>% filter(study_start == 1) # all start dates included

ss <- b %>%
  select(id, datetime, study_start) %>%
  filter(study_start == 1)

ch2 <- ch2 %>%
  left_join(ss) %>% 
  replace_na(list(study_start = 0)) %>% glimpse()
  
ch2 %>% filter(study_start == 1) # good

# Remove rows prior to study_start

ch2_2 <- ch2 %>% # every variable becomes NA prior to landfall (except id which is grouping variable)
  group_by(id) %>%
  mutate(across(everything(),
                ~replace(., row_number() < match(1, study_start), NA))) %>%
  ungroup()

noID <- ch2_2 %>% # Remove rows where all vars are NA but ID
  select(animal:study_start) %>%
  filter(if_any(everything(), ~ !is.na(.)))

ch2_3 <- ch2_2 %>% # Put ID back in dataframe 
  right_join(noID)

#saveRDS(ch2_3, here("Data", "Derived-data", "DFs", "OG_ch2_denInfo.Rds"))
