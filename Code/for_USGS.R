##################################################
#######   PRESENTATION FOR TODD ATWOOD  ##########
##################################################

# Oil and Gas Summary 6-28-2023

library(gt)
library(sf)
library(adehabitatLT)
library(tidyverse)
library(lubridate)
library(conflicted)

source('./Code/MyFunctions.R')

conflicts_prefer(
  dplyr::select(),
  dplyr::filter()
)

rm(list = ls())


# ----- LOAD DATA --------------------- #



b <- readRDS('./Data/Derived-data/DFs/bears_ch2_052823.Rds')


# -------  BEAR SAMPLES ---------------- #

# Create track in adehabitatLT

bsf <- st_as_sf(b, crs = st_crs(3338))
bsf <- dplyr::select(bsf, -c('X','Y'))

bsf <- cbind(bsf, st_coordinates(bsf))

bdf <- bsf %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(id, datetime, X, Y) %>%
  na.omit()

ltraj <- as.ltraj(bdf[,c("X","Y")], date=bdf$datetime, id=bdf$id, typeII = TRUE)

summary <- summary(ltraj)

summary$DaysTrack <-round(difftime(summary$date.end, summary$date.begin, units="days"),digits=1)

summary <- summary %>%
  mutate(date.begin = as.character("date.begin")) %>%
  mutate(date.end = as.character("date.end")) %>%
  separate(date.begin, c("start_date", "start_time")) %>%
  separate(date.end, c("end_date", "end_time"))

summary <- summary %>%
  select(id, date.begin, date.end, DaysTrack) %>%
  mutate(year = year(date.begin)) %>%
  mutate(DaysTrack = as.numeric(DaysTrack)) %>%
  mutate(start_date = ymd(date.begin)) %>%
  mutate(end_date = ymd(date.end)) %>%
  arrange(year) 

kable(summary)