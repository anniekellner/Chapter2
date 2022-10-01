#######################################################################################
#####     IS THERE SOCIAL STRATIFICAION SURROUNDING THE HARVEST   #####################
#######################################################################################

rm(list = ls())

# --------------------  LOAD DATA ------------------------------------------------ #

# Harvest

harvest <- read.csv('./Data/Bonepile_Dates.csv')

# Bonepile
bpt <- readRDS('./Data/Derived-data/DFs/Space_Use_Summaries/time_at_bonepile.Rds')

## To get summary of time spent, have to add 20735's stints together and remove second entry

bpt$time_spent <- as.numeric(bpt$time_spent)
bpt[17,4] <- sum(bpt[17,4] + bpt[18,4])

bpt[-18,] -> bpt

bpt2 <- bpt %>%
  select(id, time_spent) %>%
  mutate(where = "Bonepile") %>%
  dplyr::rename(time = time_spent) # because also in Plyr so gives error