##########################################################
########    DENNING DATES AND LOCATIONS   ################
##########################################################

# TA data only goes up to 2011
# 1 bear in common: pb_21237.2011
# 2 bears recorded as denning after 2011 (pb_21015.2013 and pb_21368.2014)
# 1 bear not included from 2008 (pb_20333.2008)

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(sf)
library(adehabitatLT)
library(plotly) # loads ggplot2
library(tmap)
library(tmaptools)

rm(list = ls())

source('./Code/MyFunctions.R') # for splitting geom into columns and getting mode values

# --- LOAD DATA ------------- #

den <- read.csv('./Data/Denning_locs_dates.csv') # denning data from TA

b <- readRDS('./Data/Derived-data/DFs/bears_ch2_093022.Rds') # cut off at Nov 1

all <- readRDS('./Data/Derived-data/DFs/all_v2.Rds') # all data (df)

harvest <- read.csv('./Data/Bonepile_Dates.csv')

bpt <- readRDS('./Data/Derived-data/DFs/Space_Use_Summaries/time_at_bonepile.Rds')

bones <- st_read('./Data/Spatial/Bonepiles/bonepiles.shp')

# ---  PREP DATA ------------ #

# my data

bearIDs <- unique(b$id)

dbears <- b %>%
  group_by(id) %>%
  filter(repro == "enter_den") %>%
  slice_head()

# denning data

den$entrance <- mdy(den$entrance)
den$year <- year(den$entrance)

den2 <- den %>% # add column for id
  mutate(X = str_trim(X, side = "right")) %>% # remove whitespace
  unite("id", X, year, sep = '.') %>%
  filter(id %in% ids)

# all data (with dates beyond Nov 1)

all <- all %>%
  mutate(gps_lat = round(gps_lat, digits = 3)) %>%
  mutate(gps_lon = round(gps_lon, digits = 3))

# harvest data

harvest$Dates <- mdy(harvest$Dates)
harvest$Year <- year(harvest$Dates)

years <- unique(b$year)

h <- harvest %>%
  filter(Year %in% years) %>%
  filter(!(Bonepile == "Barrow")) %>%
  mutate(Bonepile = recode(Bonepile, "Cross " = "Cross")) # Because there was a space in csv file and wouldn't join correctly

tz(h$Dates) <- 'US/Alaska'

  
# --- DETECT DENNING LOCATION BY USING MODE LOC VALUES  ---------- #

# See if I can detect denning signature in 2011 bear (-155.103, 71.13)

x <- all %>% # get bear for which I have denning location from TA
  filter(id == "pb_21237.2011" & month > 8) 

Mode(x$gps_lat) # 71.12931 - check
Mode(x$gps_lon) # -155.1067 - check (very slightly off but fine)

sum(x$gps_lat == 71.129) # 23
sum(x$gps_lon == -155.107) # 23


# ---------   CHECK OTHER DENNING BEARS -------------------------------- #

unique(dbears$id) # 20333.2008, 21015.2013, 21368.2014

# when is landfall

b %>% # can also get age info from this df
  filter(landfall == 1) 

# ------------------- CREATE NSD PLOTS  ---------------------------- #

## 20333.2008 
# definitely a denning bear; can see on NSD plot
# Landfall is on the later end: 09/09/2008

x2 <- all %>% # get bear for which I have denning location from TA
  filter(id == "pb_20333.2008" & month > 8)

traj.pb<-as.ltraj(xy=x2[,c("X","Y")], date=x2$datetime, id=as.character(x2$id))
traj.df <- ld(traj.pb)

plot_ly(data = traj.df, x = ~date, y = ~R2n, type = "scatter")

denLat <- Mode(x2$gps_lat) # this works - correct denning location
denLong <- Mode(x2$gps_lon)

x2 %>% filter(gps_lat == denLat & gps_lon == denLong) %>% arrange(datetime) %>% slice_head() # first day at den location - NOT START OF DEN PERIOD!

denPeriod <- x2 %>% filter(gps_lat == denLat & gps_lon == denLong) %>% arrange(datetime) 

## 21015.2013 
# Definitely denning bear
# bonepile_denning_info.xlsx says bear is present during all harvests. 

x2 <- all %>% 
  filter(id == "pb_21015.2013" & month > 7)

# NSD

traj.pb<-as.ltraj(xy=x2[,c("X","Y")], date=x2$datetime, id=as.character(x2$id))
traj.df <- ld(traj.pb)

plot_ly(data = traj.df, x = ~date, y = ~R2n, type = "scatter")

denLat <- Mode(x2$gps_lat) # this works - correct denning location
denLong <- Mode(x2$gps_lon)


## 21368.2014
# Denning bear - checked 10/31/22
# BEar arrived 4 days after first harvest; stays through the rest

x2 <- all %>% 
  filter(id == "pb_21368.2014" & month > 7)

# NSD

traj.pb<-as.ltraj(xy=x2[,c("X","Y")], date=x2$datetime, id=as.character(x2$id))
traj.df <- ld(traj.pb)

plot_ly(data = traj.df, x = ~date, y = ~R2n, type = "scatter")

denLat <- Mode(x2$gps_lat) # this works - correct denning location
denLong <- Mode(x2$gps_lon)

# Plot Denning location
# on the mainland near Cross Island

denLoc <- st_sfc(st_point(x = c(denLong, denLat)), crs = 4326)

tm_shape(denLoc) + 
  tm_dots(col = "red") +
  tm_shape(bones) + 
  tm_dots(col = "yellow")

#############################################################################
##  EXTRA   #################################################################


# Plot

x2 <- st_as_sf(x2, coords = c('X', 'Y'), crs = 3338)

tmap_mode('view')

tm_shape(x2) + 
  tm_symbols(col = 'month', popup.vars = 'ymd') + 
  tm_shape(bones) + 
  tm_dots(col = "purple", size = 0.25)


