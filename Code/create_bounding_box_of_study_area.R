####################################
###   Study Extent  ################
####################################

# Create bounding box of study area
# Remove points east of US-CA border

library(sf)
library(tmap)
library(dplyr)

rm(list = ls())

pb <- readRDS('./Data/bears_092921.Rds')

pb <- st_transform(pb, 4326)

coords <- st_coordinates(pb)
pb <- cbind(pb, coords)

pb <- filter(pb, X < -141)

tmap_mode('view')

tm_shape(pb) + 
  tm_dots()

bb = st_as_sfc(st_bbox(pb))

st_write(bb, './Data/Spatial/Output/bbox_for_heather.shp')

saveRDS(pb, './Data/bears_092921.Rds')
