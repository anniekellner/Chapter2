#######################################################################################
#####     IS THERE SOCIAL STRATIFICAION SURROUNDING THE HARVEST   #####################
#######################################################################################

library(lubridate)
library(dplyr)

rm(list = ls())

# --------------------  LOAD AND PREP DATA ------------------------------------------------ #

## Bonepile

bpt <- readRDS('./Data/Derived-data/DFs/Space_Use_Summaries/time_at_bonepile.Rds')

bpt$Year <- year(bpt$start)
years <- unique(bpt$Year)

bpt <- bpt %>%
  select(id, start, end, Year) %>%
  mutate(Bonepile = if_else(
    id == "pb_20492.2008" | id == "pb_20520.2012" | id == "pb_20735.2009" | id == "pb_20966.2008" | 
      id == "pb_20982.2008" | id == "pb_32282.2008" | id == "pb_32366.2011" | id == "pb_32608.2008",
    "Kaktovik", "Cross")) %>%
  mutate(t_interval = interval(start = start, end = end, tzone = 'US/Alaska'))

## Harvest

harvest <- read.csv('./Data/Bonepile_Dates.csv')

harvest$Dates <- mdy(harvest$Dates)
harvest$Year <- year(harvest$Dates)

h <- harvest %>%
  filter(Year %in% years) %>%
  filter(!(Bonepile == "Barrow")) %>%
  mutate(Bonepile = recode(Bonepile, "Cross " = "Cross")) # Because there was a space in csv file and wouldn't join correctly

tz(h$Dates) <- 'US/Alaska'

# ------------------   JOIN -------------------------------------------------- #

bpt2 <- bpt %>%
  full_join(h, by = "Bonepile") %>% 
  dplyr::rename(c(harvest_date = Dates, harvest_year = Year.y, Bear_year = Year.x)) %>%
  mutate(t_interval = interval(start, end)) 


sameDay <- bpt2 %>%
  mutate(within_t = harvest_date %within% t_interval)





# Scratch code:


h2 <- h %>%
  dplyr::mutate(h, twoDays = interval(Dates + days(2)))


i <- interval(test, test + days(2)) # sweet - this works







