#####################################################
###   DETERMINE STUDY START DATES   #################
#####################################################

# see ice_arrive_depart.csv for info from Chapter 1
# No bears from 2009 in this study were collared on land

library(dplyr)
library(lubridate)
library(sf)
library(tidyr)

rm(list = ls())

# ----- LOAD DATA ---------------------- #

all <- readRDS('./Data/Derived-data/DFs/all.Rds')

b <- readRDS('./Data/Derived-data/DFs/bears_ch2_093022.Rds') # ch 2 data

s <- b %>% # landfall OR start date
  select(id, landfall, study.start, date, datetime) %>%
  filter(landfall == 1 | study.start == 1)

# --------  Bears that made landfall ---------------------------- #

lf <- s %>%
  filter(landfall == 1) # 13 bear-years (11 individuals; 2 bears swam in two separate years)

lf$date <- mdy(lf$date)
lf$ordinal <- yday(lf$date)
summary(lf$ordinal)

lf %>% arrange(ordinal)

lf <- rename(lf, start_date = date)


# ------------ Bears that were collared on land ---------------------- #

c <- s %>%
  filter(landfall == 0 & study.start == 1) # 8 bears collared on land (all in 2008)

c$date <- mdy(c$date) 

c$start_date <- c$date + ddays(5) # Add five days to account for capture effects

c$ordinal <- yday(c$start_date)

summary(c$ordinal)

c %>% arrange(ordinal)

c2 <- select(c, id, start_date)

cids <- unique(c2$id)

# --------------- REJOIN TO DATABASE  ---------------------------------- #

b2 <- b %>%
  st_drop_geometry() %>%
  filter(id %in% cids)

b3 <- b2 %>%
  left_join(c2) %>%
  select(-study.start)

b4 <- b3 %>%
  group_by(id, ymd) %>%
  slice_head() %>%
  mutate(study.start = 
           if_else(ymd == start_date, 1, 0)) 

b <- select(b, -study.start) # most recent ch2 df

ch2 <- b %>%
  left_join(b4) %>%
  select(-start_date)

ch2 <- ch2 %>%
  mutate(study.start = 
           if_else(landfall == 1, 1, study.start)) 

ch2 <- ch2 %>%
  replace_na(list(study.start = 0))

ch2 %>% filter(study.start == 1) # looks good 
  

# -------- Combine into 1 start date df for summary stats ----------------- #

lf2 <- select(lf, id, start_date, ordinal)
c2 <- select(c, id, start_date, ordinal)

sd <- rbind(lf2, c2)

summary(sd)

sd %>% arrange(ordinal)
