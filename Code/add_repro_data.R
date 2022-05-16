##############################################
##    ADD REPRO DATA TO RSF DF'S  ############
##############################################

library(dplyr)
library(sf)

rm(list = ls())

source('Code/MyFunctions.R')

# ----------  Load data -------------------- #

# Chapter 1 data

ch1 <- readRDS('C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/ch1_landing/data/derived-data/all.Rds')

repro <- select(ch1, id, repro)

repro <- repro %>%
  group_by(id) %>%
  slice_head()

# Chapter 2 data

corr <- readRDS('./Data/Derived-data/corridor_data.Rds')
bone <- readRDS('./Data/Derived-data/bonepile_data.Rds')

# ----  Join  ---------------------------- #

corr2 <- st_drop_geometry(corr)
corr2 <- corr2 %>% left_join(repro)

bone2 <- st_drop_geometry(bone)
bone2 <- bone2 %>% left_join(repro)

# Add geometry data back into df's

bone3 <- left_join(bone2, bone)
corr3 <- left_join(corr2, corr)

# ---------- Save -------------------- #

saveRDS(bone3, './Data/Derived-data/bonepile_data.Rds')
saveRDS(corr3, './Data/Derived-data/corridor_data.Rds')
