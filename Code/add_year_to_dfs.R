#####################################################
##    WHAT YEARS DO DATA REPRESENT?   ###############
#####################################################

library(dplyr)
library(lubridate)
library(tidyr)

rm(list = ls())

# ------ LOAD DATA ---------------- #

corr <- readRDS('./Data/Derived-data/corridor_data.Rds')
bone <- readRDS('./Data/Derived-data/bonepile_data.Rds')

# Get years

corr$year <- year(corr$t1_)

bone2 <- bone %>%
  separate(id, c(NA, "year"), sep = '\\.', remove = FALSE)

# Save new df's

saveRDS(corr, './Data/Derived-data/corridor_data.Rds')
saveRDS(bone2, './Data/Derived-data/bonepile_data.Rds')

unique(corr$year)
unique(bone2$year)
