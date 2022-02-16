#####################################################
##    LAND COVER  ###################################
#####################################################

# Categorize vegetation type. 
# Column we want is 'COMM' - reference codes to 'aga_arctic_ak_geobotanical_side1.jpg' 
# https://arcticatlas.geobotany.org/catalog/dataset/alaska-arctic-tundra-vegetation-map-raynolds-et-al-2006

library(sf)
library(dplyr)
library(tmap)
library(tmaptools)

rm(list = ls())

# ------------ Load data  -------------------------------- #

land <- st_read('C:/Users/akell/Documents/PhD/Polar_Bears/Data/veg/aga_arctic_ak_geobotanical.shp') # veg shp

bb <- st_read('./Data/Spatial/Bounding_box/rectangle.shp')

corr <- readRDS('./Data/Derived-data/corridor_data.Rds')
bone <- readRDS('./Data/Derived-data/bonepile_data.Rds')


# Prep Data

land <- st_transform(land, 3338)
land <- select(land, COMM:geometry)

bb <- st_bbox(bb)

land <- st_crop(land, bb)


# ----------  Extract Values ------------------------------------------ #

corr.veg <- st_intersection(corr, land) # only returns rows with data
bone.veg <- st_intersection(bone, land)


# -----   Plot  -------------------------------------------------------- #

tmap_mode('view')

tm_shape(land) + 
  tm_polygons(col = "COMM") + 
  tm_layout(legend.outside = TRUE) + 
  tm_shape(bone.veg) + 
  tm_symbols(size = 0.5,popup.vars = c("COMM"))

# ----- Join dataframes ----------------------------------------------- #

# Convert sf objects to dataframes so can join

corr <- as.data.frame(corr)
bone <- as.data.frame(bone)
corr.veg <- as.data.frame(corr.veg)
bone.veg <- as.data.frame(bone.veg)

corr2 <- left_join(corr, corr.veg)
bone2 <- left_join(bone, bone.veg)

# LOOKS GOOD!

# ----- Consolidate categories -------- #

unique(corr2$COMM)
unique(bone2$COMM) # No veg in bonepile points!

corr3 <- corr2 %>%
  mutate(
    veg = case_when(
    COMM == "W2.2" | COMM == "W1.2" | COMM == "W1.1" | COMM == "W2.1" ~ "Wetlands",
    COMM == "G3.1" | COMM == "G3.3" ~ "Non-tussock tundra",
    COMM == "G4.1" | COMM == "G4.3" ~ "Tussock tundra",
    COMM == "S1.1" ~ "Dwarf-shrub tundra"
  ))

corr3 <- st_as_sf(corr3)
  
# ---- Map again  ----- #

tm_shape(land) + 
  tm_polygons(col = "COMM") + 
  tm_layout(legend.outside = TRUE) + 
  tm_shape(corr3) + 
  tm_symbols(size = 0.5,popup.vars = c("veg"))

# Looks great 2/16/22
