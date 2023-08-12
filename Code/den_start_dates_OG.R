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

rm(list = ls())

# ---- LOAD AND PREP DATA  ----------------- #

b <- readRDS("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/Chapter2/Data/Derived-data/DFs/ch2_no_end_cutoff_080823.Rds")

ch2IDs <- unique(b$id)

all <- readRDS("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/Chapter2/Data/Derived-data/DFs/all_052323.Rds") # has denning start dates

den <- read_csv('./Data/Denning_locs_dates.csv') %>%
  rename(animal = ...1) 

den$entrance_ymd <- mdy(den$entrance)
den$year <- year(as.character(den$entrance_ymd)) 

den <- den %>% 
  unite("id", c("animal", "year"), sep = '.')

denIDs <- unique(den$id)

oldCh2 <- readRDS('./Data/Derived-data/DFs/bears_ch2_110622.Rds') # to see if there are bears in new pag data that are denning bears


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

newBear <- b %>%
  filter(id == "pb_21264.2011")

allDen <- allDen %>%
  full_join(newBear) 

denCh2 <- den %>%
  select(id:Den_long, Produced_coy) %>%
  filter(id %in% allDenIDs)
  

  


  




