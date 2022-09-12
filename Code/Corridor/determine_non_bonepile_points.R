####################################################
##    ISOLATE NON-BONEPILE POINTS   ################
####################################################

library(dplyr)
library(tidyr)
library(tmap)
library(tmaptools)

rm(list = ls())

source('./Code/MyFunctions.R') # for st_drop_geometry

# Read in data

bone <- readRDS('./Data/all_bonepile_points.Rds')
all <- readRDS('./Data/bears_092921.Rds')

all <- all %>%
  select(id, datetime, geometry) %>%
  filter(id == "pb_20525.2013" | id == "pb_20586.2008" | id == "pb_06810.2008" | 
           id == "pb_20333_2008" | id == "pb_21368.2014" | id == "pb_32282.2008" | 
           id == "pb_32608.2008" | id == "20414.2009" | id == "pb_21237.2011")

bp$bonepile <- 1

bone <- bp

# Change both sf objects to regular dataframes

all <- cbind(all, st_coordinates(all)) # separate coords from geometry columns into X and Y columns
all <- st_drop_geometry(all)
alldf <- as.data.frame(all)

bone <- cbind(bone, st_coordinates(bone))
bone <- st_drop_geometry(bone)
bonedf <- as.data.frame(bone)

# Join dataframes

all2 <- all %>%
  left_join(bonedf) %>%
  replace_na(list(bonepile = 0))


# Plot corridor points

all.sf <- st_as_sf(all2, coords = c('X', 'Y'), crs = 3338) # convert to sf object for plotting

corr <- dplyr::filter(all.sf, bonepile == 0) 

tmap_mode('view')

tm_shape(corr) + 
  tm_symbols(col == "id")

#saveRDS(nobp, './Data/all_non_bonepile_pts.Rds')
