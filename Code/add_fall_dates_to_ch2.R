#########################################################
###   ADD BACK DATES TO CH2 DF  #########################
#########################################################

library(dplyr)
library(sf)
library(lubridate)

rm(list= ls())

# -----   LOAD DATA   ------------------- #

all <- readRDS('./Data/Derived-data/DFs/all_11_06_2022.Rds') # all data (df)
ch2 <- readRDS('./Data/Derived-data/DFs/bears_ch2_110622.Rds') # study data

# Add back all fall dates to ch2 df

ch2ids <- unique(ch2$id)

allCh2 <- all %>%
  filter(id %in% ch2ids & month > 9) 

allCh2$ymd <- ymd(allCh2$ymd) # Because ymd is chr in all df

ch2.2 <- ch2 %>%
  full_join(allCh2)

ch2.2 %>% group_by(id) %>% slice_tail()

#saveRDS(ch2.2, file = './Data/Derived-data/DFs/bears_ch2_052323.Rds')