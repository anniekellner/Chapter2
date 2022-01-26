####################################################
##    ISOLATE NON-BONEPILE POINTS   ################
####################################################

library(dplyr)
library(tidyr)

rm(list = ls())

source('./Code/MyFunctions.R') # for st_drop_geometry

# Read in data

bone <- readRDS('./Data/all_bonepile_points.Rds')
all <- readRDS('./Data/bears_092921.Rds')

all <- select(all, id, datetime, geometry)
bone$bonepile <- 1

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

nobp <- filter(all2, bonepile == 0)

saveRDS(nobp, './Data/all_non_bonepile_pts.Rds')
