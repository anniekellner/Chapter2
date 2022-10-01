#######################################################################################
#####     IS THERE SOCIAL STRATIFICAION SURROUNDING THE HARVEST   #####################
#######################################################################################

library(lubridate)
library(dplyr)

rm(list = ls())

# --------------------  LOAD AND PREP DATA ------------------------------------------------ #

## Bonepile

bpt <- readRDS('./Data/Derived-data/DFs/Space_Use_Summaries/time_at_bonepile.Rds')

bpt$year <- year(bpt$start)
years <- unique(bpt$year)

bpt <- bpt %>%
  select(id, start, end, year) %>%
  mutate(bonepile = if_else(
    id == "pb_20492.2008" | id == "pb_20520.2012" | id == "pb_20735.2009" | id == "pb_20966.2008" | 
      id == "pb_20982.2008" | id == "pb_32282.2008" | id == "pb_32366.2011" | id == "pb_32608.2008",
    "Kaktovik", "Cross"
  )) %>%
  mutate(t_interval = interval(start = start, end = end, tzone = 'US/Alaska'))

## Harvest

harvest <- read.csv('./Data/Bonepile_Dates.csv')

# ------------------   JOIN -------------------------------------------------- #






