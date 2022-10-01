#######################################################################################
#####     IS THERE SOCIAL STRATIFICAION SURROUNDING THE HARVEST   #####################
#######################################################################################

library(lubridate)
library(dplyr)
library(tidyr)
library(sf)

rm(list = ls())

# --------------------  LOAD AND PREP DATA ------------------------------------------------ #

## Repro and age data

r <- readRDS('./Data/Derived-data/DFs/bears_ch2_093022.Rds') # for repro and age data

r <- st_drop_geometry(r)

r <- r %>%
  group_by(id) %>%
  select(id, age, repro) %>%
  slice_head()

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
  mutate(within_t = harvest_date %within% t_interval) %>% glimpse()
  group_by(id) %>%
  slice_head() %>%
  left_join(r)

sameDay <- sameDay[-9,] # remove second entry for 20735.2009. within_t was the same (FALSE for both) 

overlap <- sameDay %>%
  filter(within_t == TRUE) %>% 
  group_by(id) %>%
  slice_head() %>%
  left_join(r) %>%
  glimpse()

## Every bear that was at the bonepile on the day of the harvest had dependent young, with the exception of one bear that is a subadult
## Run a statistical test on this result
# NA = subadult

table(sameDay$within_t)

sameDay$dependent <- ifelse(sameDay$repro == "coy" | sameDay$repro == "yearling", 1, 0)

adults <- sameDay %>%
  drop_na(repro)

fisher.test(adults$within_t, adults$dependent)

table(r$repro, useNA = "always") # repro = all bears in study (both bonepile and non)
table(sameDay$repro, useNA = "always") # sameDay = bonepile bears only
table(overlap$repro, useNA = "always") # overlap = bears are present at bonepile on at least one harvest 

r$dependent <- ifelse(r$repro == "coy" | r$repro == "yearling", 1, 0)
table(r$dependent)

overlap$dependent <- ifelse(overlap$repro == "coy" | overlap$repro == "yearling", 1, 0)
table(overlap$dependent)

# Scratch code:


h2 <- h %>%
  dplyr::mutate(h, twoDays = interval(Dates + days(2)))


i <- interval(test, test + days(2)) # sweet - this works







