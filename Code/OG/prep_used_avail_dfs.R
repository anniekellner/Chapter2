#######################################################################
########    COMBINE USED AND AVAILABLE DATAFRAMES   ###################
#######################################################################

# From exported amt data, separate id, X, Y, 

library(tidyverse)
library(here)
library(conflicted)

conflicts_prefer(
  dplyr::filter()
)

rm(list = ls())

#   ---------  LOAD AND PREP DATA   ------------------  #

# Load data as exported from amt
  # SSF Columns: 
          # id - bear ID 
          # burst - movement "burst"
          # x1 - starting x
          # x2 - ending x
          # y1  - starting y 
          # y2  - ending y
          # sl - step length
          # ta - turning angle
          # t1 - starting time
          # t2 - ending time
          # dt - time difference between points
          # case - used (TRUE) or available (FALSE) point
          # step_id - not totally sure

ssf2 <- readRDS(here("Data", "Derived-data", "DFs", "OG", "ssf_2h_ua.Rds"))
ssf4 <- readRDS(here("Data", "Derived-data", "DFs", "OG", "ssf_4h_ua.Rds"))
rsf <- readRDS(here("Data", "Derived-data", "DFs", "OG", "bonepile_pts_ua_090323.Rds")) # no time aspect 

# Create ID vectors for use later when separating for analysis

ssf2$analysis <- "ssf2hr"
ssf4$analysis <- "ssf4hr"
rsf$analysis <- "rsf"

## Combine into one dataframe

# Combine ssf analyses and rename columns

ssf2 <- ssf2 %>%
  select(id, x2_, y2_, case_, analysis) 

ssf4 <- ssf4 %>%
  select(id, x2_, y2_, case_, analysis) 

allssf <- rbind(ssf2, ssf4)

allssf <- allssf %>%
  mutate(used = 
           if_else(case_ == TRUE, 1, 0)) %>%
  rename(X = x2_) %>%
  rename(Y = y2_) %>%
  select(-case_)
  
# Rename RSF columns

rsf <- rsf %>%
  mutate(used = 
           if_else(case_ == TRUE, 1, 0)) %>%
  rename(X = x_) %>%
  rename(Y = y_) %>%
  select(-case_) %>% glimpse()

# Combine all points into single dataframe

allUA <- rbind(allssf, rsf)
  
#saveRDS(allUA, here("Data", "Derived-data", "DFs", "OG", "allUA.Rds"))


