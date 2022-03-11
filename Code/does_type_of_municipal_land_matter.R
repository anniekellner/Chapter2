#########################################################
###   DOES THE TYPE OF MUNICIPALITY MATTER?   ###########
#########################################################

# Do bears avoid some types of municipal places but not others? 
# Conduct an iSSA to see whether bears avoid some types of municipalities and not others

# Different types of municipal layers:
# North Slope Villages = city centers for Kaktovik, Deadhorse, etc. 
# Native Lands NPRA - Native territory within NPRA
# Native Allotments_NS - individually owned Native-held lands
# Cabins - hunting/fishing camp cabins for subsistence activities
# Village BND - municipal boundaries of Kaktovik, Barrow, Niuqsut, etc. 

rm(list = ls())

library(sf)
library(dplyr)
library(amt)
library(tmap)
library(tmaptools)


source('./Code/MyFunctions.R')

# --------  DATA ----------------------   #

## Bears

corr <- readRDS('./Data/Derived-data/corridor_data.Rds')
corr.sf <- st_as_sf(corr, crs = 3338)

corr.pl <- slice_sample(corr.sf, prop = .05, replace = FALSE) # So it doesn't take too long to plot

# Make unique step ID's so can run at population level 

corr.sf$unique_step_id <- paste(corr.sf$id, corr.sf$step_id_, sep = "_")


# Villages

vill <- st_read('C:/Users/akell/OneDrive - Colostate/PhD/Chapter2/Data/NSB GIS/NS_Villages.shp')
vill <- st_transform(vill, 3338)

# Native Allotments

na1 <- st_read('./Data/Spatial/Municipal/NSB/Native_Allotments_NS.shp')
na2 <- st_read('./Data/Spatial/Municipal/Native_lands_and_boundaries/native_allotments_east/NativeAllotments.shp')

native_allot <- st_join(na1, na2)
  
# Native lands
  
native_muni <- st_read('./Data/Spatial/Municipal/NSB/Native_lands_NPRA.shp')

# Cabins

cabins <- st_read('./Data/Spatial/Municipal/Native_lands_and_boundaries/native_cabins/cabins.shp')

# ---- Viz ------------------------- #

tmap_mode('view')

tm_shape(vill) + 
  tm_symbols(col = "purple", shape = 0) + 
  tm_shape(native_allot) + 
  tm_symbols(col = "green", shape = 3) + 
  tm_shape(native_muni) + 
  tm_polygons(col = "yellow") + 
  tm_shape(cabins) + 
  tm_symbols(col = "blue", shape = 6) + 
  tm_shape(corr.pl) + 
  tm_symbols(size = 0.1, alpha = 0.5)

# ---- ANALYSIS --------------------------- #

## Do bears interact with villages?
## NO

vill_500m <- st_buffer(vill, 500)
vill_1km <- st_buffer(vill, 1000)
vill_5km <- st_buffer(vill, 5000)

corr.sf2 <- corr.sf %>%
  mutate(village_buffer500m = lengths(st_intersects(., vill_500m)) > 0) %>%
  mutate(village_buffer1km = lengths(st_intersects(., vill_1km)) > 0 ) %>%
  mutate(village_buffer5km = lengths(st_intersects(., vill_5km)) > 0 )

## Result: bears do not interact with village centers. Very few points are within 1 km of village centers

## Do bears interact with native cabins?
## NO

cab_500m <- st_buffer(cabins, 500)

corr.sf3 <- corr.sf %>%
  mutate(cabins = lengths(st_intersects(., cab_500m)) > 0)

which(corr.sf3$cabins == TRUE)

## Do Municipal boundaries matter?







