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
  slice_head() %>%
  mutate(age_class = 
         case_when(
           age < 5 ~ "Subadult",
           TRUE ~ "Adult"
         ))

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
  mutate(t_interval = interval(start, end)) %>%
  glimpse()


mutate(age_class = 
         case_when(
           age < 5 ~ "Subadult",
           TRUE ~ "Adult"
         ))

sameDay <- bpt2 %>%
  mutate(within_t = harvest_date %within% t_interval) 

# Which bears were not present on any harvest date

none <- r %>%
  anti_join(sameDayT, by = "id") %>%
 mutate(overlap = "FALSE")

table(none$age_class) # 2 of the 3 subadults waited to visit the bonepile (how long did they wait?)


r2 <- r %>%
  left_join(none) %>%
  replace_na(list(overlap = "TRUE")) %>%
  print(n = 21)

fisher.test(r2$age_class, r2$overlap)  # result is not significant

  

sameDayF <- dplyr::filter(sameDay, within_t == FALSE) %>%
  group_by(id) %>%
  slice_head() %>%
  left_join(r)

table(sameDayF$repro) # Doesn't appear to be a pattern
table(sameDayF$age_class) # Doesn't appear to be a pattern


fisher.test(sameDay$within_t, sameDay

table(r$repro, useNA = "always") # repro = all bears in study (both bonepile and non)
table(sameDayT$repro, useNA = "always") # sameDay = bonepile bears only


r$dependent <- ifelse(r$repro == "coy" | r$repro == "yearling", 1, 0)
table(r$dependent)

overlap$dependent <- ifelse(overlap$repro == "coy" | overlap$repro == "yearling", 1, 0)
table(overlap$dependent)

# Scratch code:


h2 <- h %>%
  dplyr::mutate(h, twoDays = interval(Dates + days(2)))


i <- interval(test, test + days(2)) # sweet - this works







