############################################################################
#########   DETERMINE USED AND AVAILABLE POINTS FOR CORRIDOR RSF ###########
############################################################################

library(amt)
library(dplyr)
library(sf)
library(tmap)
library(tmaptools)

rm(list = ls())

source('./Code/MyFunctions.R')

# -------------   LOAD AND FORMAT DATA   ------------------------ #

cor <- readRDS('./Data/Derived-data/corridor_data.Rds')

cor <- filter(cor, case_ == "TRUE") 

## Make track with amt

track <- make_track(cor, x1_, y1_, crs = 3338, id = id) # makes one big track with all animals
trk <- track %>% nest(data = -"id") # creates dataframes for each individual

  
  
  