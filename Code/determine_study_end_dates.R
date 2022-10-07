#######################################################
########    STUDY END DATES   #########################
#######################################################

# Sent an email to the group on 9/21/22 about study start and end dates
# Only SB responded
# Seems reasonable to start at 'landfall' - see clean_data.R

library(dplyr)
library(sf)

rm(list = ls())

# ---- LOAD DATA  ----------------- #

pb <- readRDS('./Data/Derived-data/DFs/bears_ch2_093022.Rds') # study data
all <- readRDS('./Data/Derived-data/DFs/all_v2.Rds') # all data


last <- pb %>% 
  st_drop_geometry() %>%
  group_by(id) %>%
  slice_tail() %>%
  select(animal:second, id, ymd, datetime, landfall, age, repro, study.start)

# All bears end date = October with one bear ending in early September. Why?

ids <- unique(pb$id)

all2 <- all %>%
  filter(id %in% ids) %>%
  filter(month > 8) %>%
  group_by(id) %>%
  slice_tail()