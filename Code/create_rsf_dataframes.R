#############################################################
##    CREATE RSF DATAFRAME    ###############################
#############################################################

# Create dataframe with 20 random (false) points for every true point

library(amt)
library(dplyr)

rm(list = ls())

# -------   BONEPILE  ---------------------------------- #

mcp <- readRDS('./Data/Spatial/MCPs/mcp_bonepiles.Rds')

pts <- readRDS('./Data/all_bonepile_points.Rds')
pts <- dplyr::select(pts, id, geometry)
#saveRDS(pts, file = './Data/all_bonepile_points.Rds')

ids <- unique(pts$id)