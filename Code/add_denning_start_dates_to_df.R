################################################################
###   ADD DENNING START DATES TO DATAFRAME  ####################
################################################################

# id <- 'pb_XXXXX.XX' does not work for some reason

library(dplyr)
library(sf)

rm(list = ls())

source('./Code/MyFunctions.R') # for Mode fxn

# --- LOAD DATA ------------- #

all <- readRDS('./Data/Derived-data/DFs/all.Rds') # all data (df)
ch2 <- readRDS('./Data/Derived-data/DFs/bears_ch2_110622.Rds') # study data

dbears <- ch2 %>%
  group_by(id) %>%
  filter(repro == "enter_den") %>%
  slice_head()

ids <- unique(dbears$id)

# --- PREP DATA ------------------- #

dbears <- data.frame(id = character(), 
                     denLat = double(), 
                     denLon = double())

for(i in 1:length(ids)){
  bear = subset(all, id == ids[[i]] & month > 9)
  dbears[i,1] = ids[[i]]
  dbears[i,2] = Mode(bear$gps_lat)
  dbears[i,3] = Mode(bear$gps_lon)
}




# Make sf

denLoc <- st_point(x = c(denLat, denLon))
geom <- st_geometry(denLoc)


