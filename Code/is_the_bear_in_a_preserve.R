###########################################################
###   IS THE BEAR IN A PROTECTED NATURAL AREA?  ###########
###########################################################

library(sf)
library(dplyr)
library(tmap)
library(tmaptools)


rm(list = ls())

# ----------  DATA  ---------------------------------------- #

# Spatial

anwr1 <- st_read('./Data/Spatial/ANWR/RefugeBoundary_Coast.shp')
anwr2 <- st_read('./Data/Spatial/ANWR/ArcticWildernessBoundary.shp')

anwr <- st_join(anwr1, anwr2) # join ANWR shp's into single file

npra <- st_read('./Data/Spatial/NPRA/NPRA_Planning_Areas.shp')

# Bears

bone <- readRDS('./Data/Derived-data/bonepile_data.Rds')
corr <- readRDS('./Data/Derived-data/corridor_data.Rds')

corr.sf <- st_as_sf(corr)

# ----- ANALYSIS --------------------------------------------- #

# Bonepile Bears

in_anwr = lengths(st_intersects(bone, anwr)) > 0 
bone <- cbind(bone, in_anwr)

in_npra = lengths(st_intersects(bone, npra)) > 0 
bone <- cbind(bone, in_npra)

bone2 <- bone %>%
  mutate(nature_preserve = case_when(
    in_anwr == "TRUE" ~ "TRUE",
    in_npra == "TRUE" ~ "TRUE",
    TRUE ~ "FALSE"
  ))

bone2 <- select(bone2, -c(in_anwr, in_npra))

# Corridor Bears

in_anwr = lengths(st_intersects(corr.sf, anwr)) > 0 
corr.sf <- cbind(corr.sf, in_anwr)

in_npra = lengths(st_intersects(corr.sf, npra)) > 0 
corr.sf <- cbind(corr.sf, in_npra)

corr.sf2 <- corr.sf %>%
  mutate(nature_preserve = case_when(
    in_anwr == "TRUE" ~ "TRUE",
    in_npra == "TRUE" ~ "TRUE",
    TRUE ~ "FALSE"
  ))

corr.sf2 <- select(corr.sf2, -c(in_anwr, in_npra))

# ------- PLOT  ----------------------------------------- #



