#######################################################
###   Correlate Departures and Arrivals   #############
###   See whether Arrivals fit a normal distribution  #
#######################################################

library(dplyr)

rm(list = ls())

# Landing Points

all.v2 <- readRDS('./Data/Derived-data/all_v2.Rds')

start <- all.v2 %>%
  filter(start.swim == 1) %>%
  select(id, gps_lat, gps_lon) %>%
  

end <- all.v2 %>%
  filter(end.swim == 1) %>%
  select(id, gps_lat, gps_lon) %>%
  rename(end_lon = gps_lon) %>%
  rename(end_lat = gps_lat)

# Which observation is in start list but not end list

start.ids <- unique(start$id)
end.ids <- unique(end$id)

setdiff(start.ids, end.ids) 
  
start <- start %>%
  filter(id != "pb_20525.2014") %>%
  rename(start_lon = gps_lon) %>%
  rename(start_lat = gps_lat)

start.end <- full_join(start, end)

cor.test(start.end$start_lon, start.end$end_lon)

# Do arrivals fit a normal distribution

hist(end$end_lon)

shapiro.test(end$end_lon)

mean(end$end_lon)

