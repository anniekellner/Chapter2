####################################
##    STEP LENGTH  #################
####################################

# 10/18/2021

rm(list = ls())

library(dplyr)
library(ggplot2)
library(amt)
library(sf)
library(tidyr)
library(lubridate)

# Load and prep data

pb <- readRDS('./Data/bears_092921.Rds')

fix <- readRDS('./Data/Derived-data/akde_list.Rds')# to divide animals into groups according to median fix rate
fix <- dplyr::select(fix, id, median_fix)

pb <- inner_join(pb, fix)

coords <- st_coordinates(pb) # Get coords from sf object geometry and put into columns
pb <- cbind(pb, coords)

ones <- filter(pb, median_fix == 1)
twos <- filter(pb, median_fix == 2)
fours <- filter(pb, median_fix == 4)
eights <- filter(pb, median_fix == 8)

tr1 <- make_track(ones, X, Y, datetime, id = id, crs = sp::CRS("+init=epsg:3338")) # Make track with entire dataset
tr2 <- make_track(twos, X, Y, datetime, id = id, crs = sp::CRS("+init=epsg:3338"))
tr4 <- make_track(fours, X, Y, datetime, id = id, crs = sp::CRS("+init=epsg:3338"))
tr8 <- make_track(eights, X, Y, datetime, id = id, crs = sp::CRS("+init=epsg:3338"))

tracks <- list(tr1,tr2,tr4,tr8)
#saveRDS(tracks, './Data/Derived-data/tracks.Rds')

head(tr1) # x and y coordinates have been converted to what looks like a new crs...not sure why

# Nest

tr1 <- tr1 %>% nest(data = -"id")
tr2 <- tr2 %>% nest(data = -"id")
tr4 <- tr4 %>% nest(data = -"id")
tr8 <- tr8 %>% nest(data = -"id")

# Resample tracks

tr1 <- tr1 %>%
  mutate(steps = map(data, function(x)
    x %>% track_resample(rate = hours(1), tolerance = minutes(10)) %>% steps_by_burst()))

tr2 <- tr2 %>%
  mutate(steps = map(data, function(x)
    x %>% track_resample(rate = hours(2), tolerance = minutes(20)) %>% steps_by_burst()))

tr4 <- tr4 %>%
  mutate(steps = map(data, function(x)
    x %>% track_resample(rate = hours(4), tolerance = minutes(40)) %>% steps_by_burst()))

tr8 <- tr8 %>%
  mutate(steps = map(data, function(x)
    x %>% track_resample(rate = hours(8), tolerance = minutes(80)) %>% steps_by_burst()))

# Check any implausible values

steps4 <- tr4 %>% select(id, steps) %>% unnest(cols = steps)
max(steps4$dt_) # Check to make sure max step length is reasonable given the skewed distribution
max(steps4$sl_)

# Step length dataframes

tr1 <- tr1 %>% select(id, steps) %>% unnest(cols = steps)
tr2 <- tr2 %>% select(id, steps) %>% unnest(cols = steps)
tr4 <- tr4 %>% select(id, steps) %>% unnest(cols = steps)
tr8 <- tr8 %>% select(id, steps) %>% unnest(cols = steps)

# Distribution of step length

tr1 <- tr1 %>% select(id, steps) %>% unnest(cols = steps) %>% 
  ggplot(aes(sl_, fill = factor(id))) + geom_density(alpha = 0.4)

tr2 %>% select(id, steps) %>% unnest(cols = steps) %>% 
  ggplot(aes(sl_, fill = factor(id))) + geom_density(alpha = 0.4)

tr4 %>% select(id, steps) %>% unnest(cols = steps) %>% 
  ggplot(aes(sl_, fill = factor(id))) + geom_density(alpha = 0.4)

tr8 %>% select(id, steps) %>% unnest(cols = steps) %>% 
  ggplot(aes(sl_, fill = factor(id))) + geom_density(alpha = 0.4)

steps <-list(tr1, tr2, tr4, tr8)

#saveRDS(steps, './Data/Derived-data/step_lengths.Rds')
