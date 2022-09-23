################################################################################
#########   CALCULATE TIME SPENT ALONG COASTAL CORRIDOR   ######################
################################################################################

library(dplyr)
library(tidyr)
library(sf)
library(lubridate)
library(adehabitatLT)

# ----------------- LOAD AND PREP DATA   ---------------------- #

pb <- readRDS('./Data/Derived-data/DFs/bears_ch2_092122.Rds')

tz <- 'US/Alaska'

corrpts <- filter(pb, at_bonepile == 0)

# Look for large lapses in time

diff <- corrpts %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(timediff = difftime(datetime, lag(datetime), units = 'days'))

which(diff$timediff > 2)

diff[4234:4235,]

# 2 is not a bonepile bear
# 9 is not a bonepile bear
# 

## 20520.2012

pb20520_1 <- corrpts %>% # Prior to bonepile 
  filter(id == "pb_20520.2012" & datetime <= as.POSIXct("2012-08-27 16:00:00", tz = tz)) %>%
  arrange(datetime) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  dplyr::select(id, datetime) %>%
  mutate(phase = ifelse(row_number()==1, "start", "end"))

pb20520_2 <- corrpts %>% # After bonepile 
  filter(id == "pb_20520.2012" & datetime >= as.POSIXct("2012-10-19 10:00:00", tz = tz)) %>%
  arrange(datetime) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  dplyr::select(id, datetime) %>%
  mutate(phase = ifelse(row_number()==1, "start", "end"))

## 20735.2009

pb20735_1 <- corrpts %>% # Prior to bonepile 
  filter(id == "pb_20735.2009" & datetime <= as.POSIXct("2009-08-09 21:00:00", tz = tz)) %>%
  arrange(datetime) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  dplyr::select(id, datetime) %>%
  mutate(phase = ifelse(row_number()==1, "start", "end"))

pb20735_2 <- corrpts %>% # After leaving bonepile and before returning 
  filter(id == "pb_20735.2009" & datetime >= as.POSIXct("2009-08-30 08:00:00", tz = tz) &
           as.POSIXct(datetime < as.POSIXct("2009-09-16 01:00:00", tz = tz))) %>%
  arrange(datetime) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  dplyr::select(id, datetime) %>%
  mutate(phase = ifelse(row_number()==1, "start", "end"))

## 20845.2015

pb20845_1 <- corrpts %>% # Prior to bonepile 
  filter(id == "pb_20845.2015" & datetime < as.POSIXct("2015-09-23 19:03:03", tz = tz)) %>%
  arrange(datetime) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  dplyr::select(id, datetime) %>%
  mutate(phase = ifelse(row_number()==1, "start", "end"))

pb20845_2 <- corrpts %>% # After bonepile 
  filter(id == "pb_20845.2015" & datetime >= as.POSIXct("2015-10-03 05:00:19", tz = tz)) %>%
  arrange(datetime) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  dplyr::select(id, datetime) %>%
  mutate(phase = ifelse(row_number()==1, "start", "end"))

## 20982.2008

pb20982_1 <- corrpts %>% # Prior to bonepile 
  filter(id == "pb_20982.2008" & datetime <= as.POSIXct("2008-09-21 07:00:00", tz = tz)) %>%
  arrange(datetime) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  dplyr::select(id, datetime) %>%
  mutate(phase = ifelse(row_number()==1, "start", "end"))

pb20982_2 <- corrpts %>% # After bonepile 
  filter(id == "pb_20982.2008" & datetime >= as.POSIXct("2008-10-05 03:00:00", tz = tz)) %>%
  arrange(datetime) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  dplyr::select(id, datetime) %>%
  mutate(phase = ifelse(row_number()==1, "start", "end"))

## 21015.2013

pb21015
