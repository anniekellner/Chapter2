############################
###   IFSAR Data  ##########
############################

# https://apps.nationalmap.gov/downloader/#/
# Interferometric Synthetic Aperture Radar

# Stars vignette: https://r-spatial.github.io/stars/articles/

# Remove tifs from zipped folders and put into tif folder

rm(list = ls())

#library(raster)
#library(stars)
library(dplyr)
library(sf)

bears <- readRDS('./Data/bears_092921.Rds')

st_write(bears, 'C:/Users/akell/Documents/PhD/Polar_Bears/Data/bears_ch2.shp')
bears <- st_transform(bears, 4326) # add lat/long coordinates for GEE
coords <- st_coordinates(bears)
bears <- cbind(bears, coords)

source('./Code/MyFunctions.R')

#bears <- st_drop_geometry(bears)
bears <- select(bears, animal:second, id, ymd, datetime, landfall)

write.csv(bears, 'C:/Users/akell/Documents/PhD/Polar_Bears/Data/bears_ch2.csv')

# ------------------------ Raster

file_list = list.files(path = "C:/Users/akell/Documents/PhD/Polar_Bears/Data/ifsar/tifs", 
                       pattern = '.tif', 
                       recursive = TRUE) # Desktop

# Reclassify anything below 0 as NA (to eliminate water pixels and maybe save memory during processing)

for(i in 1:length(file_list)){
  r = raster(paste0('C:/Users/akell/Documents/PhD/Polar_Bears/Data/ifsar/tifs/', file_list[i]))
  rc = reclassify(r, cbind(-Inf, 0, NA), right=TRUE)
  writeRaster(rc, filename = paste0('C:/Users/akell/Documents/PhD/Polar_Bears/Data/ifsar/reclassified_rasters/', 
                                    "rc_", file_list[i]), 
                                    format = "GTiff", 
                                    overwrite = TRUE)
}

names(r_list) <- c("x", "y")
r_list$filename <- 'test.tif'
r_list$overwrite <- TRUE
m <- do.call(merge, r_list)

# ------------------ Stars

file_list = list.files(path = "C:/Users/akell/Documents/PhD/Polar_Bears/Data/ifsar/tifs", pattern = '.tif', full.names = TRUE)
test <- file_list[1:2]

y = read_stars(test, proxy = TRUE, along = c('x','y'))

l <- list()

for(i in 1:length(file_list)){
  l[[i]] <- read_stars(file_list[i])
}

test <- Reduce(c, l)
