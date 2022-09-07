############################################################################
#########   DETERMINE USED AND AVAILABLE POINTS FOR CORRIDOR RSF ###########
############################################################################

library(sf)
library(dplyr)

rm(list = ls())

# ---------- LOAD DATA  ------------------------ #

pts <- readRDS('./Data/Derived-data/corridor_data.Rds')

pts <- filter(pts, case_ == "TRUE")

pts <- as_tibble(pts)

pts$dist_to_coast <- as.numeric(pts$dist_to_coast)

max(pts$dist_to_coast, na.rm = TRUE) # 27.2 km
mean(pts$dist_to_coast, na.rm = TRUE) # 2 km  
sd(pts$dist_to_coast, na.rm = TRUE)
