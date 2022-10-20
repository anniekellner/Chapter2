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

b <- readRDS('./Data/Derived-data/DFs/bears_ch2_093022.Rds')

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

# See if I can detect denning signature in 2011 bear (-155.103, 71.13)

x <- b %>%
  st_as_sf(crs = 3338) %>%
  st_transform(4326) %>%
  filter(id == "pb_21237.2011") %>%
  geom_into_columns()

Mode(x$X)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
