############################################################
########    GET MISSING BEARS FROM   #######################  
########       PAGANO DATA           #######################
############################################################

# see ice_arrive_depart.csv for info from Chapter 1
# No bears from 2009 in this study were collared on land


library(tidyverse)
library(sf)
library(conflicted)

conflicts_prefer(
  dplyr::filter()
)

rm(list = ls())

# --------    LOAD DATA   --------------------  #

pag <- read_csv("Data/Pagano_bears.csv")

all <- readRDS("Data/Derived-data/DFs/all_052323.Rds")

ch2 <- readRDS('./Data/Derived-data/DFs/bears_ch2_052823.Rds')


# Figure out which bears are different btw Pag and me

ch2_id_yr <- ch2 %>% 
  select(animal, year) %>%
  separate_wider_delim(animal, delim = "_", names = c(NA, "ID"), cols_remove = TRUE) %>%
  select(ID, year) %>%
  rename(YEAR = year) %>%
  distinct() %>%
  mutate(ID = as.double(ID)) %>%
  ungroup()

ch2_id_yr <- select(ch2_id_yr, ID, YEAR)

dif <- setdiff(pag, ch2_id_yr) 

# --------- ADD MISSING BEARS ----------------------- #

missing <- all %>%
  filter(id == "pb_20446.2009" | # ice bear - arrival 7/31/2009 11:00:00
           id == "pb_20529.2004" | # ice bear - arrival ?
           id == "pb_20529.2005" | # ice bear - arrival 
           id == "pb_20965.2008" | # probably collared on land
           id == "pb_20975.2008" | # probably collared on land
           id == "pb_21264.2011" | # ice bear
           id == "pb_21358.2013") %>%
  as_tibble()

saveRDS(missing, "Data/Derived-data/DFs/missing_bears.Rds")


