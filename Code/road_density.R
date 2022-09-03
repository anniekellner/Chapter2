####################################################
#########   ROAD DENSITY - RESIDENTIAL  ############
####################################################

## NEED TO FIGURE OUT WHETHER ROADS ARE RESIDENTIAL OR INDUSTRIAL

# Tiger roads were obtained from TIGER US Census Roads from the 2016 release. 
# Acquired via Google Earth Engine 2/17/22.
# See TIGER_road_codes_mtfccs_2016.pdf for road codes
  # S1400 = local neighborhood road, rural road, city street
  # S1500 = 4WD vehicular trail (these may be service roads for oil/gas)
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

# ------- DATA  ------------------------ #

## TIGER roads from GEE

tig <- st_read('./Data/Spatial/Roads/tiger-roads.shp') %>% st_transform(3338)
unique(tig$mtfcc)

## NSRoads_V10 from NSSI

st_read('./NSRoads')

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

