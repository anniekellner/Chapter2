#####################################################################
####    SPACE USE ANALYSIS    #######################################
#####################################################################

library(dplyr)

rm(list - ls())

# ------- LOAD AND PREP DATA ------------------------------------------------- #

pb <- readRDS('./Data/bears_092921.Rds')

## Add 'bear type' (bonepile, bonepile only, natural)

pb2 <- pb %>%
  mutate(bear_type = case_when(
    
  ))