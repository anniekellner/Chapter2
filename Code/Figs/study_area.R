#################################################################
########    CHAPTER 2: STUDY AREA FIG   #########################
#################################################################

rm(list = ls())

library(sf)
library(dplyr)


# ----  LOAD DATA ----------------------------------------------------------------------------------------------- #

dem_poly <- st_read('C:/Users/akell/OneDrive - Colostate/Documents/ArcGIS/North_Slope_DEM/dem_poly/dem_poly.shp')

land <- dem_poly %>%
  filter(gridcode != 27) %>%
  st_union() 

# Landing Points

all.v2 <- readRDS('./data/derived-data/all_v2.Rds') # to get landing points

end.swim <- all.v2 %>%
  dplyr::filter(end.swim == 1) %>%
  dplyr::select(animal, year, month, day, hour, minute, second, gps_lat, gps_lon, datetime, start.swim, end.swim, id, X, Y, ymd, id.datetime)

coords.end <- cbind(end.swim$X, end.swim$Y) 
end.sp <- SpatialPointsDataFrame(coords = coords.end, data = end.swim, proj4string = projection)
end.sf <- st_as_sf(end.sp)

end.sf <- st_transform(end.sf, crs = polar)

centroid_landing <- st_coordinates(end.sf) %>%
  as.data.frame() %>%
  summarise(across(.cols = everything(), .fns = mean)) %>%
  SpatialPoints() %>%
  st_as_sf() %>%
  st_set_crs(polar)