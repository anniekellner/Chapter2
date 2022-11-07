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

rm(list = ls())

# ---- LOAD DATA  ----------------- #

ch2 <- readRDS('./Data/Derived-data/DFs/bears_ch2_110622.Rds') # study data
all <- readRDS('./Data/Derived-data/DFs/all.Rds') # all data


