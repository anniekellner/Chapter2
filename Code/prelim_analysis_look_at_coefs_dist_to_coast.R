#######################################################
###   PRELIMINARY ANALYSES      #######################
###   DISTANCE TO COAST         #######################
#######################################################

## What do coefficients look like when using + and - designations for onshore/offshore in distance to coast metric 

library(dplyr)

rm(list =ls())

# ----- Data  ----------------------------------------- #

corr <- readRDS('./Data/Derived-data/corridor_data.Rds')

corr <- corr %>%    # remove previous column and replace with new once since adding buffer to islands
  select(-on_island) %>%
  rename(on_island = on_island.1)


