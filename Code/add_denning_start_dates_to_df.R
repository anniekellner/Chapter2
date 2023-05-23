################################################################
###   ADD DENNING START DATES TO DATAFRAME  ####################
################################################################

# id <- 'pb_XXXXX.XX' does not work for some reason

library(dplyr)
library(sf)
library(lubridate)
library(tidyr)
library(stringr)


rm(list = ls())

source('./Code/MyFunctions.R') # for Mode fxn

# --- LOAD DATA ------------- #

all <- readRDS('./Data/Derived-data/DFs/all_11_06_2022.Rds') # all data (df)
ch2 <- readRDS('./Data/Derived-data/DFs/bears_ch2_110622.Rds') # study data
denTA <- read.csv('./Data/Denning_locs_dates.csv') # denning data from TA

# --- PREP DATA ------------------- #

# TA

denTA$entrance <- mdy(denTA$entrance)
denTA$year <- year(denTA$entrance)

# Ch2 data

dbears <- ch2 %>%
  group_by(id) %>%
  filter(repro == "enter_den") %>%
  slice_head()

ids <- unique(dbears$id)

denTA <- denTA %>% # add column for id
  mutate(X = str_trim(X, side = "right")) %>% # remove whitespace
  unite("id", X, year, sep = '.') %>%
  filter(denTA, id %in% ids)

denLocs <- data.frame(id = character(), 
                     denLat = double(), 
                     denLon = double())

#   --------- GET DENNING LOCATIONS ----------------------------------  #

# Get denning locations using Mode method

for(i in 1:length(ids)){
  bear = subset(all, id == ids[[i]] & month > 9)
  denLocs[i,1] = ids[[i]]
  denLocs[i,2] = Mode(bear$gps_lat)
  denLocs[i,3] = Mode(bear$gps_lon)
}

denLocs <- denLocs %>%
  mutate(across(where(is.numeric), round, 1)) # need to round to 1 digit because otherwise GPS error will throw off denning start date

## Location for my bears matches TA location for 21237.2011

# Join denLocs to dbears

#dbears <- denLocs %>%
  #right_join(dbears) %>%
  #filter(id %in% ids & month > 9) %>%
  #mutate(across(where(is.numeric), round, 1))


#t <- t %>%
  #group_by(id) %>%
  #mutate(den_location = 
           #if_else(gps_lat == denLat & gps_lon == denLon, 1, 0)) #%>%
  #select(-c('denLat', 'denLon')) %>%

# Get all locations for denning bears

allDen <- all %>%
  filter(id %in% ids) %>%
  filter(den_location == 1)

time <- t %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(time.den=ifelse(den_location==0 | is.na(lag(den_location)) | lag(den_location)==0, 
                          0,
                          difftime(datetime, lag(datetime), units="days"))) %>% 
  mutate(cumtime.den=cumsum(time.den))

# Create new df for manual inspection and addition of denning start dates
  
dstart <- time %>% 
  filter(den_location == 1 & cumtime.den < 5)
  
dstart$enter_den <- NA  

dstart[1,34] <- 1  # 20333.2008
dstart[38,34] <- 1 # 21015.2013
dstart[91,34] <- 1 # 21368.2014
dstart[87,34] <- 1 #21237.2011 - TA data says bear entered den 11-10-2011 but that was probably UTC or something. No data for that date. 

ds <- dstart %>%
  select(id, animal:second, den_location, enter_den)

# ------------ ADD BACK TO MAIN DATAFRAME --------------------------- #

all2 <- all %>%
  left_join(ds) 

all3 <- all2 %>%
  replace_na(list(den_location = 0, enter_den = 0))

#saveRDS(all3, './Data/Derived-data/DFs/all_11_06_2022.Rds')

##. THIS CODE COPIED FROM A DIFFERENT SCRIPT SO OBJECT NAMES WILL NEED TO CHANGE TO CALCULATE
## Dates when bears enter dens

ch2all$Ordinal <- yday(ch2all$ymd)

denDate <- ch2all %>% # n = 4
  filter(enter_den == 1) %>%
  select(Ordinal)

mean(denDate$Ordinal) # 307.25
min(denDate$Ordinal) # 277
max(denDate$Ordinal) #332

