##################################################
##    CHECK DATA: ISLAND/WATER/MAINLAND ##########
##################################################

library(sf)
library(dplyr)
library(tmap)
library(tmaptools)

rm(list = ls())

# --- Load Data -------------------- #

# Bear data

corr <- readRDS('./Data/Derived-data/corridor_data.Rds')
bone <- readRDS('./Data/Derived-data/bonepile_data.Rds')

corr <- corr %>%
  mutate(terrain = case_when(
    on_island == TRUE ~ "island",
    in_water == TRUE ~ "water",
    TRUE ~ "mainland"
  ))

# Spatial data

buf <- st_read('./Data/Spatial/islands_w_1500m_buffer.shp')

# --- Plot  -------------------------- #

tmap_mode('view')

tm_shape(corr) + 
  tm_symbols(col = "terrain") + 
  tm_shape(buf) + 
  tm_polygons(col = "#E6C5F6")