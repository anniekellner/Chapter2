#######################################################
###     BARRIER ISLANDS   ############################
######################################################

# Two barrier island shapefiles created (via ArcGIS Pro and GEE) - need to merge and reconcile projections

rm(list = ls())

library(sf)
library(tmap)
library(tmaptools)

# ------  Merge barrier island shapefiles --------------------------------------------------- #

# Shapefile provided by T. Atwood (island features only)
# Trying to figure out which projection works best

bi1 <- st_read('./Data/Spatial/Barrier_islands/barrier-islands.shp') # from GEE
bi_aa <- st_read('./Data/Spatial/Barrier_islands/barrier_islands1.shp') # from ArcGIS - this is better


bi2 <- st_read('./Data/Spatial/Barrier_islands/Barrier-islands2.shp')

tm_shape(bi2) + 
  tm_polygons() # look better

# see what projection original shapefile is in

coast <- st_read('./Data/Spatial/coastline/coastline/Physical Features - Coast - Alaska Coast - 250,000_POLYGON.shp')
