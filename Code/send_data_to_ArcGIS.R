#####################################################
###   SEND TO ARCGIS  ###############################
#####################################################

# Script for creating shapefiles to send to ArcGIS

library(sf)

rm(list = ls())


corr <- readRDS('./Data/Derived-data/corridor_data.Rds')

corr <- select(corr, id, Point_ID, geometry)

st_write(corr, 'C:/Users/akell/OneDrive - Colostate/Documents/ArcGIS/Projects/Chapter2/Bear_Locations/corr_used_avail.shp')
