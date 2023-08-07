########################################################################
##########    DETERMINE START DATES FOR NEW DATASET  ###################
########################################################################

# August 2023
# Include Pagano bears and new start dates for landfall

library(tidyverse)
library(sf)
library(conflicted)

conflicts_prefer(
  dplyr::filter()
)

rm(list = ls())

# ------ LOAD AND PREP DATA  ------------------------------- #

ch2ice <- readRDS("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/Chapter2/Data/Derived-data/DFs/ch2_bears_with_Pag_all_days.Rds")

allCh2 <- readRDS("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/Chapter2/Data/Derived-data/DFs/bears_ch2_052823.Rds")

iceIDs <- unique(ch2ice$id)

landCollar <- allCh2 %>%
  filter(!id %in% iceIDs)