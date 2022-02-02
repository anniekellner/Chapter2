#####################################################################
###   REMOVE POINTS EAST OF US-CANADA BORDER  #######################
#####################################################################

library(dplyr)
library(sf)
library(tmap)
library(tmaptools)

rm(list = ls())

source('./Code/MyFunctions.R') # for st_drop_geometry

# Load data

nbp <- readRDS('./Data/all_non_bonepile_pts.Rds') # load all non-bonepile points
nbp <- st_as_sf(nbp, coords = c('X', 'Y'), crs = 3338)

ll <- st_transform(nbp, 4326) 
ll <- cbind(ll, st_coordinates(ll))

ca <- dplyr::filter(ll, X > -141) # Points in Canada

# Convert to df because anti-join does not work on sf objects

ll <- st_drop_geometry(ll)
ca <- st_drop_geometry(ca)

us <- dplyr::anti_join(ll, ca)

# Plot

us <- st_as_sf(us, coords = c('X', 'Y'), crs = 4326)

tmap_mode('view')

tm_shape(us) + 
  tm_symbols()

# Save 

saveRDS(us, './Data/all_non_bonepile_pts.Rds')





