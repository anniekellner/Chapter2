#########################################
#####   ADD REPRO DATA TO DATAFRAME #####
#########################################

library(dplyr)

rm(list = ls())

# ------ Load and prep data  -------------- #

pb <- readRDS('./Data/Derived-data/DFs/bears_ch2_091922.Rds')
repro <- readRDS('./Data/Derived-data/DFs/repro.Rds') # From Chapter 1

ids <- unique(pb$id)

repro2 <- repro %>%
  filter(id %in% ids) %>%
  select(id, repro)

# ---- Join ------------------------------- #

pb2 <- pb %>%
  full_join(repro2)

pb2 %>%
  group_by(id) %>%
  slice_head() %>%
  select(id, repro, age) %>%
  print(n = 21)