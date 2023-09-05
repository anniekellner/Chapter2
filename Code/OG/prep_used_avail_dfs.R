#######################################################################
########    COMBINE USED AND AVAILABLE DATAFRAMES   ###################
#######################################################################

library(tidyverse)
library(sf)
library(here)
library(conflicted)

rm(list = ls())

#   ---------  LOAD AND PREP DATA   ------------------  #

ssf2 <- readRDS(here("Data", "Derived-data", "DFs", "OG", "ssf_2h_ua.Rds"))
ssf4 <- readRDS(here("Data", "Derived-data", "DFs", "OG", "ssf_4h_ua.Rds"))
rsf <- readRDS(here("Data", "Derived-data", "DFs", "OG", "bonepile_pts_ua_090323.Rds"))


allssf <- rbind(ssf2, ssf4)



coast <- st_read(here("Data", "Spatial", "coastline", "digitized_coastline.shp"))
coast <- st_transform(coast, 3338)
