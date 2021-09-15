#######################################
##    Clean Dataset  ##################
#######################################

library(dplyr)
library(sf)
library(sp)
library(raster)
library(tmap)
library(lubridate)

rm(list = ls())

source('./Code/MyFunctions.R')

# -------   DATA  ----------------------------------------------------------------- #

pb <- read.csv(file = "./Data/usgs_pbear_gps_ccde16_v20170131.csv")

pb <- pb %>%
  dplyr::select(animal:date) %>% # remove unnecessary columns
  filter(month > 6 & month < 11)

pbsf <- DFtoSF(pb, 3338)
pb.spdf <- as_Spatial(pbsf)

dem <- raster('./Data/Spatial/ans_dem_8bit.tif')

for(i in 1:nrow(pb.spdf)){
  pb.spdf$temp[i] = extract(dem, pb.spdf[i,], na.rm = TRUE)
  pb.spdf$land[i] = ifelse(pb.spdf$temp[i] == 27, 0, 1)
}

# Why so many NA's?
# Because have not filtered out 'ice bears', so points are extending beyond dem
# Apply 7-day criteria

pbsf2 <- st_as_sf(pb.spdf) 

pbsf2$id = paste(pbsf2$animal, pbsf2$year, sep = '.')
pbsf2$ymd <- mdy(pbsf2$date)
pbsf2$datetime <- ymd_hms(paste(pbsf2$year, 
                                pbsf2$month, 
                                pbsf2$day, 
                                pbsf2$hour, 
                                pbsf2$minute,
                                pbsf2$second, sep = '-'), tz = "US/Alaska")

land.pts <- pbsf2 %>%
  group_by(id, ymd) %>%
  arrange(id, datetime) %>%
  mutate(number.land = cumsum(land))

saveRDS(pbsf2, file = './Data/bears_091521.Rds')


tmap_mode('view')

tm_shape(pbx) + 
  tm_symbols(col = "month", popup.vars = "land")


# Bear data

pb <- pb %>%
  dplyr::select(animal:land, id, X:id.datetime) %>%
  filter(month > 6 & month < 11) 

# Is land_bear_ows the same as land == 1? No - somehow land_bear_ows removed points not on land

water <- filter(b, land == 0)

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
