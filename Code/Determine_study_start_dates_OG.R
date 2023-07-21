####################################################################
########    DETERMINE STUDY START DATES   ##########################  
########       INCLUDE PAGANO DATA           #######################
####################################################################

# see ice_arrive_depart.csv for info from Chapter 1
# No bears from 2009 in this study were collared on land


library(tidyverse)
library(conflicted)

# --------    LOAD DATA   --------------------  #

pag <- read_csv("Data/Pagano_bears.csv")

all <- readRDS("Data/Derived-data/DFs/all_052323.Rds")

ch2_nov22 <- readRDS('./Data/Derived-data/DFs/bears_ch2_110622.Rds') # 21 bears
ch2_may23 <- readRDS('./Data/Derived-data/DFs/bears_ch2_052823.Rds')

novIDs <- unique(ch2_nov22$id)
mayIDs <- unique(ch2_may23$id) # May and Nov df's are identical for ID's



# Figure out which bears are different btw Pag and me

ch2_id_yr <- ch2 %>% 
  select(animal, year) %>%
  separate_wider_delim(animal, delim = "_", names = c(NA, "ID"), cols_remove = TRUE) %>%
  select(ID, year) %>%
  rename(YEAR = year) %>%
  distinct() %>%
  mutate(ID = as.double(ID)) %>%
  ungroup()

dif <- setdiff(pag$ID, mayIDs)