#######################################################
########    STUDY END DATES   #########################
#######################################################

# Sent an email to the group on 9/21/21 about study start and end dates
# Only SB responded
# When I created the first dataframe (Rds) for this study, I cut the dates off at November 1. 

# End dates:
  # Denning bears: when they enter dens 
  # Departing Bears: when they leave for ice
  # Other:
    # When collars stop transmitting data
  

library(dplyr)
library(sf)
library(lubridate)
library(tidyr)

rm(list = ls())

# ---- LOAD DATA  ----------------- #

ch2 <- readRDS('./Data/Derived-data/DFs/bears_ch2_052323.Rds') # study data
all <- readRDS('./Data/Derived-data/DFs/all_052323.Rds') # all data - has denning start dates

# -- ADD STUDY END DATES FOR DENNING BEARS  --------------- #

allDen <- all %>%
  filter(enter_den == 1)

allDen <- allDen %>%
  rename(study_end = enter_den) %>%
  select(id, datetime, study_end)

# Look at last dates in ch2 df

ch2 <- ch2 %>%
  left_join(allDen) %>%
  rename(study_start = study.start)

ch2 %>% filter(study_end == 1) # looks good- 4 obs

# ----  ADD STUDY END DATES FOR ICE BEARS ------------------- #

## Dates when bears leave for ice

#ch2ice <- filter(ch2, departure_to_ice == 1) %>% select(id, datetime, departure_to_ice) 
#allIce <- filter(all, departure_to_ice == 1) %>% select(id, datetime, departure_to_ice)

#setdiff(allIce, ch2ice) # missing 20735.2009

#which(ch2$id == "pb_20735.2009" & ch2$datetime == "2009-09-02 18:00:00")

ch2[6463,35] <- 1 # change missing departure to 1

ch2 <- ch2 %>%
  group_by(id) %>%
  mutate(study_end= 
           if_else(departure_to_ice == 1, 1, study_end))

# Check

table(ch2$study_end) # 12 2023-05-23

## ----------  Collar drops/malfunctions ------------------- ##

iceden <- ch2 %>% # bears that either den or depart for ice
  filter(study_end == 1) 

icedenIDS <- unique(iceden$id)
  
others <- ch2 %>%
  filter(!(id %in% icedenIDS))

otherIDS <- unique(others$id)

collarDrops <-  others %>% # Add study_end = 1 for bears whose collars dropped (data ends in September or October)
  group_by(id) %>% 
  arrange(id, datetime) %>%
  slice_tail() %>%
  mutate(study_end = replace(study_end, month == 9 | month == 10, 1)) %>%
  mutate(collar_drop = if_else(month == 9 | month == 10, 1, 0)) %>%
  select(id, datetime, collar_drop)

collarDrops <- collarDrops %>%
  filter(collar_drop == 1) 

# Join to ch2 df

ch2 <- ch2 %>%
  full_join(collarDrops) %>%
  select(-study_end)

ch2 <- ch2 %>%
  group_by(id) %>%
  mutate(study_end = if_else(departure_to_ice == 1 | enter_den == 1 | collar_drop , 1, 0))
  
table(ch2$study_end) # 19 - looks good

# ----  DETERMINE END DATE FOR REMAINING TWO BEARS  ----------- #

# Remaining bears: "pb_20418.2005" "pb_32366.2014"

# Take average ordinal study_end date for denning, departing, and collar-drop bears

ch2 <- ch2 %>%
  mutate(ordinal_date = yday(datetime)) 

se <- ch2 %>% filter(study_end == 1)
mean(se$ordinal_date) # 296 - October 21 in non-leap year
mean(se$month)
mean(se$day)
# Get remaining bears
seIDs <- unique(se$id)
setdiff(unique(ch2$id), seIDs)

# Set study end date as Oct 31 for remaining bears (306)

which(ch2$id == "pb_20418.2005" & ch2$ordinal_date > 306)
ch2[16112,39] <- 1 # change study_end to 1 (pb_20418.2005)

which(ch2$id == "pb_32366.2014" & ch2$ordinal_date == 305)

ch2[24581,39] <- 1 # change study_end to 1 (pb_32366.2014)

table(ch2$study_end) # 21 study end dates!

# ---   REMOVE DATES AFTER STUDY END DATES  ---------- #

ch2 <- ch2 %>%
  group_by(id) %>%
  mutate(row = row_number()) %>%
  mutate(across(everything(),
                ~replace(., row > match(1, study_end), NA))) %>%
  drop_na(animal) %>%
  ungroup()

#saveRDS(ch2, file = './Data/Derived-data/DFs/bears_ch2_052823.Rds')




