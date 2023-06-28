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
  mutate(year = year(date.begin)) %>%
  mutate(date.begin = as.character(date.begin)) %>%
  mutate(date.end = as.character(date.end)) %>% 
  separate_wider_delim(date.begin, " ", names = c("start_date", NA)) %>%
  separate_wider_delim(date.end, " ", names = c("end_date", NA))

summary <- summary %>%
  select(year, id, start_date, end_date, DaysTrack,) %>%
  rename(days_tracked = DaysTrack) %>%
  arrange(year) %>% print(n = 21)

summary %>%
  gt() %>%
  tab_header(title = "Bears Included in Study") %>%
  tab_style(style = cell_text(size = "small"), 
            locations = cells_body(columns = everything(), rows = everything())) %>%
  tab_style(style = cell_fill(color = "yellow"), 
                              locations = cells_body(columns = everything(), rows = c(14,16,18,20))) 
  


