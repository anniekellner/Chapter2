##########################################################################
###     RE-RUN DATES OF DEPARTURE TO ICE    ##############################
##########################################################################

# Aug 26, 2023
# OG bears

library(tidyverse)
library(sf)
library(terra)
library(here)
library(conflicted)

conflicts_prefer(
  dplyr::filter(),
  terra::extract()
)

# ----  LOAD AND PREP CH2 DATA ----------------- #

# Bears

b <- readRDS(here("Data", "Derived-data", "DFs", "OG_ch2_082623.Rds"))

# Spatial

demPoly_5k <- st_read(here("Data", "Spatial", "DEM", "AK_CA_5kbuff", "AK_CA_5kbuff.shp")) 

rec <- st_read(here("Data", "Spatial", "Rectangle", "Rectangle.shp"))
rec <- st_transform(rec, st_crs(demPoly_5k))

demPoly_5k_crop <- st_crop(demPoly_5k, rec)

buff5k <- demPoly_5k_crop %>%
  filter(OBJECTID == 14) # excludes Canada