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

# ------------------   JOIN -------------------------------------------------- #

bpt2 <- bpt %>%
  full_join(h, by = "Bonepile") %>% 
  dplyr::rename(c(BP_date = Dates, BP_year = Year.y, Bear_year = Year.x)) 

# Scratch code:

bpt.t <- bpt %>% 
  mutate(interval = interval(start, end))

h$interval <- 
  
  t <- bpt.t %>%
  pivot_wider(names_from = interval, values_from = )

h2 <- h %>%
  dplyr::mutate(h, twoDays = interval(Dates + days(2)))

test <- ymd("2008-05-13")

i <- interval(test, test + days(2)) # sweet - this works

test %within% i # good





