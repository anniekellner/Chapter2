#######################################################
########    STUDY END DATES   #########################
#######################################################

# Sent an email to the group on 9/21/21 about study start and end dates
# Only SB responded
# When I created the first dataframe (Rds) for this study, I cut the dates off at November 1. 

# End dates:
  # Denning bears: when they enter dens - FIGURE OUT WHY DISCREPANCY
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


#all <- as_tibble(all)
#all$ymd <- ymd(all$ymd)

ch2all <- ch2 %>%
  inner_join(all)

# Get IDs for bears that enter dens in all vs. ch2 df

denCh2 <- ch2all %>%
  select(enter_den = 1) %>%
  distinct()

denAll <- dplyr::select(all, enter_den = 1)

denCh2IDs <- distinct(denCh2, id)
## Dates when bears enter dens

ch2all$Ordinal <- yday(ch2all$ymd)

denDate <- ch2all %>% # n = 4
  filter(enter_den == 1) %>%
  select(Ordinal)

mean(denDate$Ordinal) # 307.25
min(denDate$Ordinal) # 277
max(denDate$Ordinal) #332

## Dates when bears leave for ice

ice <- ch2all %>% filter(departure_to_ice == 1) # 7 observations

ice$ordinal <- yday(ice$ymd)
summary(ice$ordinal)


# ---- ADD STUDY_END COLUMN TO DATAFRAME AND REMOVE POINTS AFTERWARD  ------------------ #

# not sure where study_end dates came from, so deleting and re-doing. Many dates in November/December 

ch2all <- ch2all %>%
  select(!study_end)

ch2all <- ch2all %>%
  group_by(id) %>%
  mutate(study_end = ifelse(departure_to_ice == 1 | enter_den == 1, 1, 0)) 

# Check
table(ch2all$study_end) # 12 2023-05-22

## ----------  Collar drops/malfunctions ------------------- ##

iceden <- ch2all %>% # bears that either den or depart for ice
  filter(study_end == 1) 

icedenIDS <- unique(iceden$id)
  
others <- ch2all %>%
  filter(!(id %in% icedenIDS))

otherIDS <- unique(others$id)

collarDrops <-  others %>% # Add study_end = 1 for bears whose collars dropped (data ends in September or October)
  group_by(id) %>% 
  arrange(id, datetime) %>%
  slice_tail() %>%
  mutate(study_end = replace(study_end, month == 9 | month == 10, 1))

collarDropIDS <- collarDrops %>% group_by(id) %>% filter(study_end == 1) 
unique(collarDropIDS$id)


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
