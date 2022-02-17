####################################################
#########   ROAD DENSITY - RESIDENTIAL  ############
####################################################

library(sf)
library(dplyr)


rm(list = ls())

# ------- DATA  ------------------------ #

roads <- st_read('C:/Users/akell/OneDrive - Colostate/PhD/Chapter2/Data/Roads/TIGER-roads/tiger-roads.shp')
roads <- st_transform(roads, 3338)


