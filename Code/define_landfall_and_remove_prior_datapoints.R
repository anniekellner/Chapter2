##############################################
##    Determine Landfall  ####################
##############################################

# 9/28/21: Landfall = first point after which bear uses land at least 1x for 7 days (DEM only; no buffer)

library(dplyr)
library(tidyr)
library(sf)
library(zoo)

rm(list = ls())

source('./Code/MyFunctions.R')

pb <- readRDS('./Data/bears_091521.Rds')

pb <- st_drop_geometry(pb)

pb <- pb %>% 
  mutate(date2 = as.Date(datetime)) %>%
  mutate_at(13, ~replace(., is.na(.), 0)) # replace land == NA values with 0 (because outside DEM)

pb <- pb %>% 
  group_by(id, ymd) %>%
  summarise(on_land = any(land == 1)) %>%
  mutate(consec_seven = rollapply(on_land, 7, all, align = 'left', fill = NA)) %>%
  ungroup() %>%
  left_join(pb)


pb <- select(pb, id, on_land, consec_seven, ymd, datetime, date)

arrival <- pb %>%
  group_by(id, consec_seven) %>%
  filter(consec_seven == TRUE) %>%
  slice_head() 

arrival$landfall <- 1

# Merge back into larger dataframe

pb2 <- pb %>%
  left_join(arrival) %>%
  mutate_at(7, ~replace(., is.na(.), 0)) # replace NA values in landfall column with 0

# Remove points prior to landfall 

pb3 <- pb2 %>%
  group_by(id) %>%
  mutate(day = row_number()) 

pb3 <- pb3 %>% 
  group_by(id) %>%
  mutate(across(everything(),
                ~replace(., row_number() < match(1, landfall), NA))) %>%
  na.omit() %>%
  ungroup()

# Merge landfall with original pb dataframe

pb4 <- pb3 %>%
  select(id, datetime, landfall)

pb5 <- inner_join(pb, pb4)

saveRDS(pb5, './Data/bears_092921.Rds')
