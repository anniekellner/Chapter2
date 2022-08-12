######################################################################################
###   SEE WHETHER TODD GIS FROM 2016 NEEDS TO BE ADDED TO ENERGY LAYER  ##############
######################################################################################

library(sf)
library(dplyr)

rm(list = ls())

# --------  LOAD DATA ----------------------------------------- #

# Hilcorp and CP

hil <- readRDS('./Data/Derived-data/Spatial/hil_all.Rds')
cp <- readRDS('./Data/Derived-data/Spatial/cp_all.Rds')

## Industry data not CP or Hilcorp
# Most directories refer to location on home desktop

fac <- st_read('./Data/Spatial/Industry_GIS/Facilities/Faciliites.shp') %>% # Checked in ArcGIS - small coastal facility
  st_transform(3338) %>%
  select(geometry) %>%
  mutate(layer = "fac")

end <- st_read('C:/Users/akell/OneDrive - Colostate/Documents/ArcGIS/GIS from Todd/Industry GIS/OIL/BP_GIS/infrastructure_eofendicott.shp') %>% 
  st_transform(3338) %>% 
  select(geometry) %>%
  mutate(layer = "endicott")

fac2 <- st_read('C:/Users/akell/OneDrive - Colostate/Documents/ArcGIS/GIS from Todd/Industry GIS/OIL/FAC.shp') %>% 
  st_transform(3338) %>%
  select(geometry) %>%
  mutate(layer = "fac2_3")

pipes <- st_read('C:/Users/akell/OneDrive - Colostate/Documents/ArcGIS/GIS from Todd/Industry GIS/OIL/PIPES.shp') %>%
  st_transform(3338) %>%
  select(geometry) %>%
  mutate(layer = "pipes")

prop_fac <- st_read('C:/Users/akell/OneDrive - Colostate/Documents/ArcGIS/GIS from Todd/Industry GIS/OIL/PROP_FAC.shp') %>%
  st_transform(3338) %>%
  select(geometry) %>%
  mutate(layer = "PROP_FAC")

roads <- st_read('C:/Users/akell/OneDrive - Colostate/Documents/ArcGIS/GIS from Todd/Industry GIS/OIL/ROADS.shp') %>%
  st_transform(3338) %>%
  select(geometry) %>%
  mutate(layer = "roads")

tiger_rds <- st_read('C:/Users/akell/OneDrive - Colostate/Documents/ArcGIS/GIS from Todd/Industry GIS/OIL/tiger_roads.shp') %>%
  st_set_crs(4326) %>% st_transform(3338) %>%
  select(geometry) %>%
  mutate(layer= "tiger roads")

# ------- See whether these shapefiles are duplicates of CP and Hilcorp ----------------------------------------------- #

hil_pipes <- st_read('./Data/Spatial/Industry_GIS/Hilcorp/Pipes_Hilcorp.shp') %>%
  select(geometry) %>%
  st_transform(3338) %>%
  mutate(company = "Hilcorp")

cp_pipes <- st_read('./Data/Spatial/Industry_GIS/CP_Infrastructure/Kuparuk_Pipelines.shp') %>%
  select(geometry) %>%
  st_transform(3338) %>%
  mutate(company = "CP")

ind_pipes <- st_union(hil_pipes, cp_pipes)

inter_pipes <- st_intersection(ind_pipes, pipes)

plot(st_geometry(inter_pipes))
