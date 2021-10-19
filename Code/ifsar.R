############################
###   IFSAR Data  ##########
############################

# https://apps.nationalmap.gov/downloader/#/
# Interferometric Synthetic Aperture Radar

# Stars vignette: https://r-spatial.github.io/stars/articles/

library(raster)
library(stars)

file_list = list.files(path = "C:/Users/akell/Documents/PhD/Polar_Bears/Data/ifsar", pattern = '.tif', full.names = TRUE) # Desktop

test <- read_stars(file_list, along = 1)

l <- list()

for(i in 1:length(file_list)){
  l[[i]] <- read_stars(file_list[i])
}

# Combine two objects

l1 <- l[[1]]
l2 <- l[[2]]

test <- c()
