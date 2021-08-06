#######################################
##    Clean Dataset  ##################
#######################################

library(dplyr)
library(sf)
library(sp)
library(raster)
library(tmap)

rm(list = ls())

source('./Code/MyFunctions.R')

# -------   DATA  ----------------------------------------------------------------- #

load('./Data/all_v2.RData')

# Bear data

b <- all.v2 %>%
  select(animal:land, id, X:id.datetime, land_bear_ows, land_bear) %>%
  filter(month > 6 & month < 11) %>%
  filter(land_bear_ows == 1)

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster

# points

coords <- cbind(b$X, b$Y)
b.spdf <- SpatialPointsDataFrame(coords = coords, data = b, proj4string = projection) 
pb <- st_as_sf(b.spdf) #convert to sf object

# Spatial data

us <- st_read('./Data/Spatial/State_Shapefile/States_NAD83_Albers.shp') # From NPS state shapefile
ak <- us %>%
  filter(STATE_ABBR == "AK") %>%
  st_transform(st_crs(pb))

plot(st_geometry(ak))
plot(st_geometry(pb), add = TRUE)

# -----   Create dataset  ------------------------- #

unique(pb$id)
table(pb$id)

# -------- Use tmap to visualize dataset  ----------------------------------- #

tmap_mode("view")

tm_shape(ak) + 
  tm_polygons() + 
  tm_shape(pb) + 
  tm_symbols(col = "month", popup.vars = c("day")) + 
  tm_facets(by = "id") + 
  tmap_options(limits = c(facets.view = 40))

# Remove bears with less than 100 data points

noData <- pb %>%
  group_by(id) %>%
  add_count(id) %>%
  filter(n < 100)

# Convert sf object to dataframe

pbdf <- st_drop_geometry(pb)
noDatadf <- st_drop_geometry(noData)
mydata <- anti_join(pbdf, noDatadf)

table(mydata$id)
unique(mydata$id) # 29
