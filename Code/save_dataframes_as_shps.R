#########################################################
###   SAVE DATAFRAMES AS SHAPEFILES FOR USE WITH GEE  ###
#########################################################

library(sf)

rm(list = ls())

# ----  Read in data  -------------------------- #

bone <- readRDS('./Data/Derived-data/bonepile_data.Rds')
corr <- readRDS('./Data/Derived-data/corridor_data.Rds')

bone_pts <- readRDS('./Data/Derived-data/bonepile_pts_used_avail.Rds')

# ----  Convert to sf object  ----------------- #

bone <- st_as_sf(bone)
corr <- st_as_sf(corr)

bone_pts <- st_as_sf(bone_pts, coords = c('x_', 'y_'), crs = 3338)

# --  Write to shapefile  ----------------------- #

st_write(bone, './Data/Derived-data/Shapefiles/bonepile_data.shp')
st_write(corr, './Data/Derived-data/Shapefiles/corridor_data.shp')

st_write(bone_pts, './Data/Derived-data/Shapefiles/bonepile_points.shp')
