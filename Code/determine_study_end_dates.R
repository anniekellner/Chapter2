#######################################################
########    STUDY END DATES   #########################
#######################################################

# Sent an email to the group on 9/21/21 about study start and end dates
# Only SB responded
# When I created the first dataframe (Rds) for this study, I cut the dates off at November 1. 

# Seems reasonable to start at 'landfall' - see clean_data.R
# End dates:
  # Denning bears: when they enter dens
  # Departing Bears: when they leave for ice
  # Other:
    # When collars stop transmitting data
  

library(dplyr)
library(sf)

rm(list = ls())

# ---- LOAD DATA  ----------------- #

#pb <- readRDS('./Data/Derived-data/DFs/bears_ch2_093022.Rds') # study data
all <- readRDS('./Data/Derived-data/DFs/all_v2.Rds') # all data


last <- pb %>% 
  st_drop_geometry() %>%
  group_by(id) %>%
  slice_tail() %>%
  select(animal:second, id, ymd, datetime, landfall, age, repro, study.start)

# All bears end date = October with one bear ending in early September. Why?

ids <- unique(pb$id)

# Is it because the data drops off on those dates?
# No

all2 <- all %>%
  filter(id %in% ids) %>%
  filter(month > 8) %>%
  group_by(id) %>%
  slice_tail()

# Look at first saved Rds file and see what final points are

old <- readRDS('./Data/Derived-data/DFs/Old/bears_072321.Rds')

old %>%   
  filter(id %in% ids) %>%
  filter(month > 8) %>%
  group_by(id) %>%
  slice_tail()
