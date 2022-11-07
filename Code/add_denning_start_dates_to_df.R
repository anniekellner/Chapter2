################################################################
###   ADD DENNING START DATES TO DATAFRAME  ####################
################################################################

# id <- 'pb_XXXXX.XX' does not work for some reason

library(dplyr)
library(sf)
library(zoo)


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

# Get denning locations using Mode method

for(i in 1:length(ids)){
  bear = subset(all, id == ids[[i]] & month > 9)
  dbears[i,1] = ids[[i]]
  dbears[i,2] = Mode(bear$gps_lat)
  dbears[i,3] = Mode(bear$gps_lon)
}

dbears <- dbears %>%
  mutate(across(where(is.numeric), round, 1))

t <- all %>%
  left_join(dbears) %>%
  filter(id %in% ids & month > 9) %>%
  mutate(across(where(is.numeric), round, 1))

t <- t %>%
  group_by(id) %>%
  mutate(den_location = 
           if_else(gps_lat == denLat & gps_lon == denLon, 1, 0)) %>%
  #select(-c('denLat', 'denLon')) %>%
  #right_join(all)

all2 %>% filter(den_location == 1)

db <- all2 %>%
  filter(id %in% ids & month > 9) %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(time.den=ifelse(den_location==0 | is.na(lag(den_location)) | lag(den_location)==0, 
                          0,
                          difftime(datetime, lag(datetime), units="days"))) %>% 
  mutate(cumtime.den=time.den + ifelse(is.na(lag(time.den)), 0, lag(time.den)))
  
db %>%
  filter(den_location == 1)
  
  

# Make sf

denLoc <- st_point(x = c(denLat, denLon))
geom <- st_geometry(denLoc)


