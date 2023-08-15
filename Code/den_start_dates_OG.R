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

allDen <- all %>%
  filter(enter_den == 1 | 
           id %in% denIDs & 
           id %in% ch2IDs) 

allDenIDs <- unique(allDen$id)

# Old Ch2 denning bears

dbears <- oldCh2 %>%
  group_by(id) %>%
  filter(repro == "enter_den") %>%
  slice_head() # 4 bears: so one needs to be added

setdiff(allDenIDs, unique(dbears$id)) # 21264.2011

newBear <- den %>%
  filter(id == "pb_21264.2011") # Entrance date = 11/5/2011 ( see OneNote for location and COY)

# Get data for all denning bears
  # oldCh2 has 'start date' but not enter_den
  # allDen has enter_den but doesn't seem right

newBear <- b %>%
  filter(id == "pb_21264.2011")

allDen <- allDen %>%
  full_join(newBear) # 5 bears: good

allDenIDs <- unique(allDen$id)


#   --------- GET DENNING LOCATIONS ----------------------------------  #
  
denLocs <- data.frame(id = character(), 
                      gps_lat = double(), 
                      gps_lon = double())

# Get denning locations using Mode method

for(i in 1:length(allDenIDs)){
  bear = subset(allDen, id == allDenIDs[[i]] & month > 9)
  denLocs[i,1] = allDenIDs[[i]]
  denLocs[i,2] = Mode(bear$gps_lat)
  denLocs[i,3] = Mode(bear$gps_lon)
}

denLocs$gps_lat <- round(denLocs$gps_lat, 2) # round to two digits for lat/long otherwise GPS error messes up dates
denLocs$gps_lon <- round(denLocs$gps_lon, 2)

# Merge to get datetimes

all_fall <- allUSGS %>%
  select(animal:rate) %>%
  filter(month > 9) %>%
  unite("id", c("animal", "year"), sep = '.', remove = FALSE) 

all_fall$gps_lat <- round(all_fall$gps_lat, 2)
all_fall$gps_lon <- round(all_fall$gps_lon, 2)

inDen <- denLocs %>%
  inner_join(all_fall)

inDen$at_densite <- 1

# Merge in_den back into all_fall to get all dates

all_fall <- all_fall %>%
  left_join(inDen) %>%
  replace_na(list(at_densite = 0))








