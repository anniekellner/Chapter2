#############################################################
#########   CREATE ROAD LAYERS  #############################
#############################################################

library(sf)
library(dplyr)
library(terra)
library(tmap)
library(tmaptools)

rm(list = ls())

source('./Code/MyFunctions.R')

######    MUNICIPAL   ########################################

# Tiger roads cover municipal areas

# Tiger roads were obtained from TIGER US Census Roads from the 2016 release. 
# Acquired via Google Earth Engine 2/17/22.
# See TIGER_road_codes_mtfccs_2016.pdf for road codes
  # S1400 = local neighborhood road, rural road, city street
  # S1500 = 4WD vehicular trail 
  # S1640 = Service Drive (these may be for servicing pipelines)
  # S1710 = Walkway/Pedestrian path (no vehicles)
  # S1740 = Private oil road
  # S1200 = Secondary road (US Hwy, AK Hwy)

## Ice roads are not included in municipal/tiger roads (checked using tmap)


# ------- DATA  ------------------------ #

## TIGER roads from GEE

tig <- st_read('./Data/Spatial/Roads/tiger-roads.shp') %>% st_transform(3338)
unique(tig$mtfcc)
table(tig$mtfcc)

# Plot to see which type of roads are where

#tmap_mode('view')

#tm_shape(tig) + 
  #tm_lines("mtfcc", lwd = 2)

tig <- tig %>%
  filter(mtfcc == "S1400" | mtfcc == "S1500" | mtfcc == "S1200") # Keeping 4WD roads because bears may select these roads for less resistance

saveRDS(tig, "./Data/Derived-data/Spatial/Roads/muni_rds.Rds")

# ------------------------------------------------------------------------------------------------------------------------------------------

## Combine ice roads so can easily remove them from other layers
  
  ## Check ice roads 

iceroad <- st_read('./Data/Spatial/Roads/IceRoads/iceroad.shp') %>%
  st_set_crs(4326) %>%
  st_transform(3338) %>%
  mutate(layer = "iceroad")  

iceroad <- select(iceroad, layer, geometry)

npra_icerd <- st_read('./Data/Spatial/Roads/IceRoads/npra_2000_2001.shp') %>%
  st_set_crs(4326) %>%
  st_transform(3338) %>%
  mutate(layer = "npra") 

npra_icerd <- select(npra_icerd, layer, geometry)

ice <- st_union(iceroad, npra_icerd, by_feature = FALSE)






