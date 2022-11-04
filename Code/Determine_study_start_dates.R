#####################################################
###   DETERMINE STUDY START DATES   #################
#####################################################

# see ice_arrive_depart.csv for info from Chapter 1
# No bears from 2009 in this study were collared on land

library(dplyr)
library(lubridate)

rm(list = ls())

# ----- LOAD DATA ---------------------- #

b <- readRDS('./Data/Derived-data/DFs/bears_ch2_093022.Rds')

s <- b %>%
  select(id, landfall, study.start, date) %>%
  filter(landfall == 1 | study.start == 1)

# Bears that made landfall

lf <- s %>%
  filter(landfall == 1) # 13 bear-years (11 individuals; 2 bears swam in two separate years)

lf$date <- mdy(lf$date)
lf$ordinal <- yday(lf$date)
summary(lf$ordinal)

lf %>% arrange(ordinal)

# Bears that were collared on land

s %>%
  filter(landfall == 0 & study.start == 1) # 8 bears collared on land (all in 2008)
