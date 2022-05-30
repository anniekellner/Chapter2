################################################
#####   ENERGY: DISTANCE TO INFRASTRUCTURE  ####
################################################

# Combine all energy infrastructure into a single entity and calculate distance to it

library(sf)
library(dplyr)

rm(list = ls())

# Read in all .shp files from a directory

cp <- list.files(path = './Data/Spatial/Industry_GIS/CP_Infrastructure', pattern = "[.]shp$", full.names = TRUE)

cp_shp <- list()

for(i in 1:length(cp)){
 shp = st_read(cp[i])
 shp = st_transform(shp, 3338)
cp_shp[[i]] = shp
}

cp2 <- bind_rows(cp_shp)
plot(st_geometry(cp2))

fac <- list.files(path = './Data/Spatial/Industry_GIS/Facilities', pattern = "[.]shp$", full.names = TRUE)
