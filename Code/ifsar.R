############################
###   IFSAR Data  ##########
############################

# https://apps.nationalmap.gov/downloader/#/
# Interferometric Synthetic Aperture Radar

library(raster)
library(stars)

file_list = list.files(path = "C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Chapter2/Data/ifsar", pattern = '.tif', full.names = TRUE)

l <- list()

for(i in 1:length(file_list)){
  l[[i]] <- read_stars(file_list[i])
}

ifsar1 <- read_stars('C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Chapter2/Data/ifsar/USGS_NED_DSM_AK_IFSAR_NPRA_Recollect_C3_2018_TIFF_2019/DSM_n7100w15515P.tif')
ifsar2 <- read_stars('C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Chapter2/Data/ifsar/USGS_NED_DSM_AK_IFSAR_NPRA_Recollect_C3_2018_TIFF_2019/DSM_n7100w15530P.tif')

plot(ifsar1)
plot(ifsar2)

ifsar <- 
