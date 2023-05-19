###############################################################
####    FIGURE OUT WHAT REPRO DATA IS EXTRAPOLATED  ###########
###############################################################

library(dplyr)
library(tidyr)

rm(list = ls())

# ----------  LOAD DATA ------------------------------- #

r <- readRDS('./Data/Derived-data/DFs/bears_ch2_093022.Rds')
csv <- read.csv('./Data/Repro-and-Age/Repro.csv') # has data on whether status was inferred or not


# ------  JOIN  ---------------------------------- #

r2 <- r %>%
  select(id, repro) %>%
  group_by(id) %>%
  slice_head()

csv2 <- csv %>%
  unite(id, c("animal", "year"), sep = '.', remove = TRUE) %>%
  select(id, Notes)
  
r3 <- r2 %>%
  left_join(csv2) # The only bear for which status has been inferred is 20525.2014