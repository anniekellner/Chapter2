####################################################
#########   ROAD DENSITY - RESIDENTIAL  ############
####################################################

## NEED TO FIGURE OUT WHETHER ROADS ARE RESIDENTIAL OR INDUSTRIAL

# Tiger roads were obtained from TIGER US Census Roads from the 2016 release. 
# Acquired via Google Earth Engine 2/17/22.
# See TIGER_road_codes_mtfccs_2016.pdf for road codes
  # S1400 = local neighborhood road, rural road, city street
  # S1500 = 4WD vehicular trail 
  # S1640 = Service Drive (these may be for servicing pipelines)
  # S1710 = Walkway/Pedestrian path (no vehicles)
  # S1740 = Private oil road
  # S1200 = Secondary road (US Hwy, AK Hwy)

# https://stackoverflow.com/questions/69035297/calculating-road-density-raster-from-road-shapefile

library(sf)
library(dplyr)
library(terra)
library(tmap)
library(tmaptools)

rm(list = ls())

source('./Code/MyFunctions.R')

# ------- DATA  ------------------------ #

## TIGER roads from GEE

tig <- st_read('./Data/Spatial/Roads/tiger-roads.shp') %>% st_transform(3338)
unique(tig$mtfcc)
table(tig$mtfcc)

# Plot to see which type of roads are where

tmap_mode('view')

tm_shape(tig) + 
  tm_lines("mtfcc", lwd = 2)

tig <- tig %>%
  filter(mtfcc == "S1400" | mtfcc == "S1500" | mtfcc == "S1200") # Keeping 4WD roads because bears may select these roads for less resistance


## NSRoads_V10 from NSSI

ns <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/North_slope_infrastructure_roads_pipelines_developed_areas/NSRoads_V10.shp') %>%
  st_transform(3338)

# Only keep roads in municipal areas



## Combine ice roads so can easily remove them from other layers

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


# --------- PLOT  ----------------------- #

tm_shape(tig) + 
  tm_lines(col = "orange") + 
  tm_shape(ns) + 
  tm_lines(col = "purple") + 
  tm_shape(ice) + 
  tm_lines(col = "blue")

## Ice roads are not included in municipal/tiger roads
## Check ice roads in industrial roads




corr <- readRDS('./Data/Derived-data/corridor_data.Rds')
bone <- readRDS('./Data/Derived-data/bonepile_data.Rds')

# --- CREATE ROAD DENSITY GRID  ----------------------- #

v <- vect(r)
roads <- as.lines(v)
rs <- rast(v)
#res(rs) <- 1000 # 1 km x 1 km 

values(rs) <- 1:ncell(rs)
names(rs) <- "rast"   
rsp <- as.polygons(rs) # raster to polygons

rp <- intersect(roads, rsp)

rp$length <- perim(rp) / 1000 #km
x <- tapply(rp$length, rp$rast, sum) # calculates the length of road in each cell 

r <- rast(rs)
r[as.integer(names(x))] <- as.vector(x)

# ---- PLOT ------------------------ #

plot(r)
lines(roads)

tm_shape(r) +  # terra objects can be plotted in tmap
  tm_raster()

