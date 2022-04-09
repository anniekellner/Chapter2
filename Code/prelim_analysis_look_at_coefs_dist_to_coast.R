#######################################################
###   PRELIMINARY ANALYSES      #######################
###   DISTANCE TO COAST         #######################
#######################################################

## What do coefficients look like when using + and - designations for onshore/offshore in disance to coast metric 

library(dplyr)

rm(list =ls())

# ----- Data  ----------------------------------------- #

corr <- readRDS('./Data/Derived-data/corridor_data.Rds')

corr <- corr %>% 
  select(-on_island) %>%
  rename(on_island = on_island.1)
