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

ch2ice <- filter(ch2, departure_to_ice == 1) %>% select(id, datetime, departure_to_ice) 
allIce <- filter(all, departure_to_ice == 1) %>% select(id, datetime, departure_to_ice)

setdiff(allIce, ch2ice) # missing 20735.2009

which(ch2$id == "pb_20735.2009" & ch2$datetime == "2009-09-02 18:00:00")

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
  mutate(study_end = replace(study_end, month == 9 | month == 10, 1))

collarDropIDS <- collarDrops %>% group_by(id) %>% filter(study_end == 1) # 7 bears whose collars stopped transmitting data in Oct or Nov
unique(collarDropIDS$id)

ch2 <- ch2 %>%
  group_by(id) %>%
  mutate(collar_drop = 
           if_else(id %in% collarDropIDS, 1, 0))

## Add collar_drop to study_end column
  
ch2 <- ch2 %>%
  group_by(id) %>%
  slice_tail() %>%
  mutate(study_end = 
           if_else(collar_drop == 1, 1, study_end))

table(ch2$study_end) 
  
## 
all2 <- all %>%
  group_by(id) %>%
  mutate(study_end = if_else(departure_to_ice == 1 | enter_den == 1, 1, 0)) %>%
  mutate(row = row_number()) %>%
  mutate(across(everything(),
                ~replace(., row > match(1, study_end), NA))) %>% # remove points for bears after which study_end = 1
  drop_na(animal) %>% # arbitrary column that does not usually have an NA for any reason - this removes rows after study_end rather than fill with NA
  ungroup()




# 7 bears whose collars appeared to drop off or malfunction



## Add back to all df

drop <- rename(drop, collar_drop = study_end) # because all df doesn't have a 'study_end' because it represents all data

all3 <- all2 %>% left_join(drop)

dropIDS <- unique(drop$id)

# Add data to bears whose collars dropped

dropAdd <- all3 %>%
  filter(id %in% dropIDS & month > 8)
  
dropAll <- all3 %>%
  filter(id %in% dropIDS & month > 8) %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  slice_tail() %>% 
  mutate(study_end = replace(study_end, 0, 1))

collarDrop <- dropAdd %>%
  full_join(dropAll)

collarDrop <- collarDrop %>%
  



dropAdd <- dropAdd %>%
  select(-c('swim', 'SIC', 'ows', 'land_bear_ows', "land_bear", 'id.ymd', 'row', 'start.swim', 'end.swim', 'ordinal'))

ch2 <- ch2 %>%
  full_join(dropAdd) 

ch2df <- ch2 %>%
  select(-'study_end')

ch2df <- ch2df %>%
  mutate(study_end = 
           case_when(enter_den == 1 ~ 1,
                     departure_to_ice == 1 ~ 1,
                     collar_drop == 1 ~ 1,
                     TRUE ~ 0))
  
ch2df %>%
  group_by(id) %>%
  slice_tail() %>%
  print(width = Inf)
