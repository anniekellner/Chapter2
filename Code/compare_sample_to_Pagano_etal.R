######################################################
#####   COMPARING MY DATA TO PAGANO ET AL. 2021 ######
######################################################

# Criteria for Pagano et al inclusion:
    # Data gap < 108 hr
    # Fix rate <= 4 hr

library(tidyverse)

rm(list = ls())

# ----------  LOAD AND PREP DATA   ----------------- #

pag <- read_csv('./Data/Pagano_bears.csv')
me <- readRDS('./Data/Derived-data/DFs/bears_ch2_052823.Rds')

me <- me %>%
  select(animal, year) %>%
  separate_wider_delim(animal, delim = "_", names = c(NA, "ID"), cols_remove = TRUE) %>%
  select(ID, year) %>%
  rename(YEAR = year) %>%
  distinct() %>%
  ungroup()

me <- me[,2:3]

me <- me %>%
  mutate(ID = as.double(ID))

dif <- setdiff(pag, me)

