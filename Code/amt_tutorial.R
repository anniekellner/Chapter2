####################################
##    AMT TUTORIAL  ################
####################################

# 10/18/2021

rm(list = ls())

library(dplyr)
library(ggplot2)
library(amt)
library(sf)
library(tidyr)

# Load and prep data

pb <- readRDS('./Data/bears_092921.Rds')
fix <- readRDS('./Data/Derived-data/akde_list.Rds')
fix <- dplyr::select(fix, id, median_fix)

# Eliminate ids fron fix that are not in pb

pb2 <- inner_join(pb, fix)

coords <- st_coordinates(pb2)
pb2 <- cbind(pb2, coords)

ones <- filter(pb2, median_fix == 1)
twos <- filter(pb2, median_fix == 2)
fours <- filter(pb2, median_fix == 4)
eight <- filter(pb2, median_fix == 8)


tr1 <- make_track(ones, X, Y, datetime, id = id, crs = sp::CRS("+init=epsg:3338"))
#tr1 <- tidyr::separate(tr1, datetime, c("date", "time"), sep = " ")

head(tr1) # x and y coordinates have been converted to what looks like a new crs...not sure why

# Step length

tr2 <- tr1 %>%
  mutate(sl = step_lengths(.))


