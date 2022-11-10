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

ch2 <- readRDS('./Data/Derived-data/DFs/bears_ch2_110622.Rds') # study data
all <- readRDS('./Data/Derived-data/DFs/all_11_06_2022.Rds') # all data - has denning start dates

## Dates when bears enter dens

ds <- all %>% filter(enter_den == 1)
ds$ordinal <- yday(ds$ymd)
summary(ds$ordinal)

ds %>% arrange(ordinal)

## Dates when bears leave for ice

ice <- all %>% filter(departure_to_ice == 1)

ice$ordinal <- yday(ice$ymd)
summary(ice$ordinal)

ice %>% arrange(ordinal)

# ---- ADD STUDY_END COLUMN TO DATAFRAME AND REMOVE POINTS AFTERWARD  ------------------ #

all2 <- all %>%
  group_by(id) %>%
  mutate(study_end = if_else(departure_to_ice == 1 | enter_den == 1, 1, 0)) %>%
  mutate(row = row_number()) %>%
  mutate(across(everything(),
                ~replace(., row > match(1, study_end), NA))) %>%
  drop_na(animal) %>% # arbitrary column that does not usually have an NA for any reason
  ungroup()

# Check
table(all2$study_end) # 12 bears depart to ice or enter dens
table(all2$enter_den)

## ------------------- Add missing rows to ch2 df ---------------------------------- ##

iceden <- all2 %>%
  filter(study_end == 1)

icedenIDS <- unique(iceden$id)

addback <- all2 %>%
  filter(id %in% icedenIDS & month > 9) 
  
ch2Cols <- colnames(ch2)
addbackCols <- colnames(addback)

setdiff(ch2Cols, addbackCols)

## Prep df to join back into ch2 df

addback$ymd <- ymd(addback$ymd)

addback <- addback %>%
  select(-c('swim', 'SIC', 'ows', 'land_bear_ows', "land_bear", 'id.ymd', 'row', 'start.swim', 'end.swim'))

ch2 <- ch2 %>%
  full_join(addback)

studyEnd <- test %>% group_by(id) %>% slice_tail()  # looks good

## ----------  Which bears end dates do not coincide with entering den or departing for ice ------------------- #

other <- studyEnd %>%
  filter(!(id %in% icedenIDS))

# Check back to all to see if more data is available

otherIDS <- unique(other$id)

otherAll <- all2 %>%
  filter(id %in% otherIDS & month > 8)

otherAll <-  otherAll %>% # Add study_end = 1 for bears whose collars dropped
  group_by(id) %>% 
  slice_tail() %>%
  mutate(study_end = 
           if_else(month == 10 | month == 9, 1, study_end)) 

# Stats summary

drop <- otherAll %>%
  filter(study_end == 1) %>%
  mutate(ordinal = yday(ymd)) %>%
  arrange(ordinal)

summary(drop$ordinal)

drop <- drop %>%
  select(-c('swim', 'SIC', 'ows', 'land_bear_ows', "land_bear", 'id.ymd', 'row', 'start.swim', 'end.swim'))

## Add back to all df

drop <- rename(drop, collar_drop = study_end) # because all df doesn't have a 'study_end' because it represents all data

all3 <- all2 %>% left_join(drop)

dropIDS <- unique(drop$id)

dropAdd <- all3 %>%
  filter(id %in% dropIDS & month > 8)


dropAll <- all3 %>%
  filter(id %in% dropIDS & month > 8) %>%
  group_by(id) %>%
  slice_tail() %>%
  mutate(study_end = replace(study_end, 0, 1))


## Prep df to join back into ch2 df

dropAdd$ymd <- ymd(dropAdd$ymd)

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
