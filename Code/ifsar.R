############################
###   IFSAR Data  ##########
############################

# https://apps.nationalmap.gov/downloader/#/
# Interferometric Synthetic Aperture Radar

# Stars vignette: https://r-spatial.github.io/stars/articles/

library(raster)
library(stars)
library(dplyr)

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

