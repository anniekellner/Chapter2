############################
###   IFSAR Data  ##########
############################

# https://apps.nationalmap.gov/downloader/#/
# Interferometric Synthetic Aperture Radar

# Stars vignette: https://r-spatial.github.io/stars/articles/

library(raster)
library(stars)
library(dplyr)

file_list = list.files(path = "C:/Users/akell/Documents/PhD/Polar_Bears/Data/ifsar", pattern = '.tif', full.names = TRUE) # Desktop

l <- list()
r_list <- list() # raster

# Read all files into a list of stars objects

for(i in 1:length(file_list)){
  l[[i]] <- read_stars(file_list[i])
}

for(i in 1:length(file_list)){
  r_list[[i]] <- raster(file_list[i])
}

ras2 <- list()


for(i in 1:length(r_list)){
  ras2[[i]] <- reclassify(r_list[[i]], cbind(-Inf, 0, NA), right=TRUE)
}

# Combine two objects

l1 <- l[[1]]
l2 <- l[[2]]

test <- c()
