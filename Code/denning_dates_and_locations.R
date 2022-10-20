##########################################################
########    DENNING DATES AND LOCATIONS   ################
##########################################################

# TA data only goes up to 2011
# 1 bear in common: pb_21237.2011
# 2 bears recorded as denning after 2011 (pb_21015.2013 and pb_21368.2014)
# 1 bear not included from 2008 (pb_20333.2008)

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(sf)

rm(list = ls())

source('./Code/MyFunctions.R') # for splitting geom into columns and getting mode values

# --- LOAD DATA ------------- #

den <- read.csv('./Data/Denning_locs_dates.csv') # denning data from TA

b <- readRDS('./Data/Derived-data/DFs/bears_ch2_093022.Rds') # cut off at Nov 1

all <- readRDS('./Data/Derived-data/DFs/all_v2.Rds') # all data (df)

# ---  PREP DATA ------------ #

# my data

bearIDs <- unique(b$id)

dbears <- b %>%
  group_by(id) %>%
  filter(repro == "enter_den") %>%
  slice_head()

# denning data

den$entrance <- mdy(den$entrance)
den$year <- year(den$entrance)

den2 <- den %>% # add column for id
  mutate(X = str_trim(X, side = "right")) %>% # remove whitespace
  unite("id", X, year, sep = '.') %>%
  filter(id %in% bearIDs)

# all data (with dates beyond Nov 1)

x <- all %>% # get bear for which I have denning location from TA
  filter(id == "pb_21237.2011" & month > 8) %>%
  mutate(gps_lat = round(gps_lat, digits = 3)) %>%
  mutate(gps_lon = round(gps_lon, digits = 3))
  
# --- DETECT DENNING LOCATION BY USING MODE LOC VALUES  ---------- #

# See if I can detect denning signature in 2011 bear (-155.103, 71.13)

Mode(x$gps_lat) # 71.12931 - check
Mode(x$gps_lon) # -155.1067 - check (very slightly off but fine)

sum(x$gps_lat == 71.129) # 23
sum(x$gps_lon == -155.107) # 23

# See about other bears - 20333.2008, 21015.2013, 21368.2014

unique(dbears$id)



