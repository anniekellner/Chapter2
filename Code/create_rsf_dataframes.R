#############################################################
##    CREATE RSF DATAFRAME    ###############################
#############################################################

# Create dataframe with 20 random (false) points for every true point

library(amt)
library(dplyr)
library(sf)

rm(list = ls())

source('./Code/MyFunctions.R')

# -------   BONEPILE  ---------------------------------- #

pts <- readRDS('./Data/all_bonepile_points.Rds') # reads in as sf object
pts <- cbind(pts, st_coordinates(pts))
pts <- st_drop_geometry(pts)

# Make track with package amt

track <- make_track(pts, X, Y, crs = sp::CRS("+init=epsg:3338"), id = id) # makes one big track with all animals
trk <- track %>% nest(data = -"id") # creates dataframes for each individual

# Create random points using 95 MCP for each individual

rsf <- data.frame(case_ = logical(), x_ = double(), y_ = double(), id = character())
ids <- unique(pts$id)

for(i in 1:length(ids)){
  bear_trk = trk$data[[i]]
  hr = hr_mcp(bear_trk)
  rsf_pts = random_points(hr, n = 20*nrow(bear_trk), presence = bear_trk)
  rsf_pts$id = ids[i]
  rsf = rbind(rsf, rsf_pts)
}

saveRDS(rsf, file = './Data/Derived-data/bonepile_pts_used_avail.Rds')

















