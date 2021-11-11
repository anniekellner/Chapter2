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

bi1 <- st_read('./Data/Spatial/Barrier_islands/barrier_islands1.shp') # from T. Atwood (AA projection)

bi2 <- st_read('./Data/Spatial/Barrier_islands/barrier-islands-11112021-2.shp') # digitized from GEE

# Reproject ifSAR-based digitized shp to AA 

bi2 <- st_transform(bi2, crs = st_crs(bi1))

islands <- st_union(bi1, bi2)

tm_shape(islands) + 
  tm_polygons()
