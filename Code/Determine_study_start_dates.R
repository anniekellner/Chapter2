#####################################################
###   DETERMINE STUDY START DATES   #################
#####################################################

# see ice_arrive_depart.csv for info from Chapter 1
# No bears from 2009 in this study were collared on land

library(dplyr)
library(lubridate)

rm(list = ls())

# ----- LOAD DATA ---------------------- #

b <- readRDS('./Data/Derived-data/DFs/bears_ch2_093022.Rds') # all bears

s <- b %>% # landfall OR start date
  select(id, landfall, study.start, date) %>%
  filter(landfall == 1 | study.start == 1)

# --------  Bears that made landfall ---------------------------- #

lf <- s %>%
  filter(landfall == 1) # 13 bear-years (11 individuals; 2 bears swam in two separate years)

lf$date <- mdy(lf$date)
lf$ordinal <- yday(lf$date)
summary(lf$ordinal)

lf %>% arrange()

lf$start_date <- lf$landfall

# ------------ Bears that were collared on land ---------------------- #

c <- s %>%
  filter(landfall == 0 & study.start == 1) # 8 bears collared on land (all in 2008)

c$date <- mdy(c$date) 
c$ordinal <- yday(c$date)

# Add five days to account for capture effects

c$start_date <- c$date + ddays(5)

# -------- Combine into 1 start date df ----------------- #

s <- rbind(lf, c)

# FINISH TONIGHT


