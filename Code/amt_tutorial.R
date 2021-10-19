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

coords <- st_coordinates(pb)
pb <- cbind(pb, coords)






tr1 <- make_track(pb, X, Y, datetime, id = id, crs = sp::CRS("+init=epsg:3338"))
#tr1 <- tidyr::separate(tr1, datetime, c("date", "time"), sep = " ")

head(tr1) # x and y coordinates have been converted to what looks like a new crs...not sure why

# Step length

tr2 <- tr1 %>%
  mutate(sl = step_lengths())

summary(tr2$sl)
zz