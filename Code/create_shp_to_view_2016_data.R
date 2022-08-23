#######################################################
###   CREATE ENERGY LAYER WITH 2016 DATA    ###########
#######################################################

# Combine roads, pipes and facilities
# Exclude overlap with industry files (CP and Hilcorp)

library(sf)
library(dplyr)

rm(list = ls())

source('./Code/MyFunctions.R') # For st_erase function

# ------------  LOAD DATA --------------------------- #

## Industry

cp <- readRDS('./Data/Derived-data/Spatial/cp_all.Rds')
hil <- readRDS('./Data/Derived-data/Spatial/hil_all.Rds')

cp$OPERATOR <- "CP"

cp <- cp %>%
  select(NAME, OPERATOR, Type, LABEL, geometry) %>%
  rename(TYPE = Type)

hil <- hil %>%
  select(NAME, OPERATOR, TYPE, LABEL, geometry)

hil$TYPE <- as.character(hil$TYPE) # Change from integer to character in order to use bind_rows()

ind <- bind_rows(cp, hil)

#saveRDS(ind, './Data/Derived-data/Spatial/Energy/cp_and_hil.Rds')

## Industry data not CP or Hilcorp

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

# --- JOIN LAYERS AND ERASE OVERLAP WITH CP/HILCORP DATA ------------------------- #

ind_2016 <- bind_rows(end, fac, fac2, pipes, prop_fac, roads, tiger_rds)

plot(st_geometry(ind_2016))



