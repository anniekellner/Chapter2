############################################################################
##    COMPARE GEE RESULTS TO RASTER PACKAGE  ###############################
############################################################################

# Output from GEE is the same as raster package


library(sf)
library(tmap)
library(tmaptools)

rm(list = ls())

corr <- st_read('C:/Users/akell/OneDrive - Colostate/PhD/Chapter2/Data/Terrain/terrain_corridor_pts.shp')

# Plot

tmap_mode('view')

tm_shape(corr) + 
  tm_symbols(popup.vars = c('elevation', 'slope'))

# Test slope against raster package
library(raster)

ras <- raster('C:/Users/akell/Documents/PhD/Polar_Bears/Data/ifsar/reclassified_rasters/rc_USGS_AK5M_AK__Summer_IfSAR_2016_D16_DTM_n7000w14345P.tif')

tm_shape(ras) + 
  tm_raster()

terr <- terrain(ras)

tm_shape(terr) + 
  tm_raster()