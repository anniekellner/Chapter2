###########################################
##    MINIMUM CONVEX POLYGON  #############
###########################################

# Create 95 MCP around points

library(sf)
library(adehabitatHR)
library(lubridate)
library(tmap)
library(tmaptools)
library(dplyr)

rm(list = ls())

# ----------------------------  Data  ----------------------------------------------- #

# Bears

pb <- readRDS('./Data/bears_092921.Rds')

pb <- st_transform(pb, 3338)
pb$datetime <- ymd_hms(pb$datetime, tz = "US/Alaska")

pbsp <- as_Spatial(pb)

mcp95 <- mcp(pbsp, percent = 95)

# Alaska

us <- st_read('./Data/Spatial/State_Shapefile/States_NAD83_Albers.shp') # From NPS state shapefile

bb <- st_read('C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears_GIS/Rectangle/Rectangle.shp')
bb <- st_transform(bb, 3338)

ak <- us %>%
  filter(STATE_ABBR == "AK") %>%
  st_transform(3338) 

ak <- st_crop(ak, bb)

islands <- st_read('./Data/Spatial/Barrier_Islands/all_islands.shp')

# Plot

tmap_mode('view')

tm_shape(islands) + 
  tm_polygons(fill = "81C97E") + 
  tm_shape(mcp95) + 
  tm_borders(col = "red") 
  #tm_shape(pb) + 
  #tm_symbols(alpha = 0.1)




