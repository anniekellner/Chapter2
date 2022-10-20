################################################################
###   ADD DENNING START DATES TO DATAFRAME  ####################
################################################################




rm(list = ls())

source('./Code/MyFunctions.R')

# --- LOAD DATA ------------- #

all <- readRDS('./Data/Derived-data/DFs/all_v2.Rds') # all data (df)

x <- all %>% # get bear for which I have denning location from TA
  filter(id == "pb_21237.2011" & month > 8) 

Mode(x$gps_lat) # 71.12931 - check
Mode(x$gps_lon) # -155.1067 - check (very slightly off but fine)