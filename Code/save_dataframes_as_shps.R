#########################################################
###   SAVE DATAFRAMES AS SHAPEFILES FOR USE WITH GEE  ###
#########################################################

library(sf)

rm(list = ls())

# ----  Read in data  -------------------------- #

bone <- readRDS('./Data/Derived-data/bonepile_data.Rds')
corr <- readRDS('./Data/Derived-data/corridor_data.Rds')

# ----  Convert to sf object  ----------------- #

bone <- st_as_sf(bone, coords = c('x_', 'y_'), crs = 3338)
corr <- st_as_sf(corr)

# --  Write to shapefile  ----------------------- #

st_write(bone, './Data/Derived-data/Shapefiles/bonepile_data.shp')
st_write(corr, './Data/Derived-data/Shapefiles/corridor_data.shp')
