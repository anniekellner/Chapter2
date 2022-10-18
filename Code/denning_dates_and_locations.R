##########################################################
########    DENNING DATES AND LOCATIONS   ################
##########################################################

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

rm(list = ls())


# --- LOAD DATA ------------- #

den <- read.csv('./Data/Denning_locs_dates.csv')
den$entrance <- mdy(den$entrance)

den$year <- year(den$entrance)

den <- den %>% # add column for id
  mutate(X = str_trim(X, side = "right")) %>% # remove whitespace
  unite("id", X, year, sep = '.')