#############################################################
##    CREATE RSF DATAFRAME    ###############################
)#############################################################

# Create dataframe with 20 random (false) points for every true point

library(amt)
library(dplyr)

rm(list = ls())

source('./Code/MyFunctions.R')

# -------   BONEPILE  ---------------------------------- #

mcp <- readRDS('./Data/Spatial/MCPs/mcp_bonepiles.Rds')

pts <- readRDS('./Data/all_bonepile_points.Rds')
pts <- dplyr::select(pts, id, geometry)
#saveRDS(pts, file = './Data/all_bonepile_points.Rds')

# Alter dataframe so matches amt results

pts$case_ <- TRUE
pts <- cbind(pts, st_coordinates(pts))
pts <- pts %>%
  st_drop_geometry() %>%
  rename(c(x_ = X, y_ = Y)) # to match FALSE pts from random_points()

ids <- unique(pts$id)
rsf <- data.frame()

for(i in 1:length(ids)){
  bear_ID = pts$id[i]
  bear_pts = dplyr::filter(pts, id == bear_ID)
  bear_mcp = dplyr::filter(mcp, id == bear_ID)
  randos = random_points(bear_mcp, n = 20*nrow(bear_pts))
  randos$id = bear_ID
  all_pts = rbind(bear_pts, randos)
  rsf = rbind(rsf, randos)
}

head(rsf)

test <- dplyr::filter(rsf, case_ == TRUE)

