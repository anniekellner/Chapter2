######################################################################################
###   SEE WHETHER TODD GIS FROM 2016 NEEDS TO BE ADDED TO ENERGY LAYER  ##############
######################################################################################

library(sf)
library(dplyr)
library(tmap)
library(tmaptools)

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

prop_fac <- st_read('C:/Users/akell/OneDrive - Colostate/Documents/ArcGIS/GIS from Todd/Industry GIS/OIL/PROP_FAC.shp') %>% # Looks like facilities and pipes?
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

## PIPES
# DUPLICATE!! Do not need to use pipes shp

hil_pipes <- st_read('./Data/Spatial/Industry_GIS/Hilcorp/Pipes_Hilcorp.shp') %>%
  select(geometry) %>%
  st_transform(3338) %>%
  mutate(company = "Hilcorp")

cp_pipes <- st_read('./Data/Spatial/Industry_GIS/CP_Infrastructure/Kuparuk_Pipelines.shp') %>%
  select(geometry) %>%
  st_transform(3338) %>%
  mutate(company = "CP")

ind_pipes <- bind_rows(hil_pipes, cp_pipes)

# Plot 

tmap_mode('view')

tm_shape(ind_pipes) + 
  tm_lines(col = "gray", lwd = 2) +
  tm_shape(pipes) + 
  tm_lines(col = "gray", lwd = 1)

## FACILITIES

kup_pads <- st_read('./Data/Spatial/Industry_GIS/CP_Infrastructure/Kuparuk_Gravel_Pads.shp') %>%
  select(geometry) %>%
  st_transform(3338) %>%
  mutate(company = "CP")
  
hilfac <- st_read('./Data/Spatial/Industry_GIS/Hilcorp/Facilities_Hilcorp.shp') %>%
  select(geometry) %>%
  st_transform(3338) %>%
  mutate(company = "Hilcorp")

ind_fac <- bind_rows(kup_pads, hilfac)

# Troubleshoot invalid geometries - https://r-spatial.org/r/2017/03/19/invalid.html

any(is.na(st_dimension(ind_fac))) # empty geometries
any(is.na(st_is_valid(ind_fac))) # corrupt geometries
any(na.omit(st_is_valid(ind_fac)) == FALSE) # invalid geometries
st_is_valid(ind_fac, reason = TRUE) # Ring Self-intersection (1x)

ind_fac <- st_make_valid(ind_fac) # Works - fixed!

oldfac <- bind_rows(fac, fac2, prop_fac)

any(is.na(st_dimension(oldfac))) # empty geometries
any(is.na(st_is_valid(oldfac))) # corrupt geometries
any(na.omit(st_is_valid(oldfac)) == FALSE) # invalid geometries
invalid <- st_is_valid(oldfac, reason = TRUE); unique(invalid) # "too few points in geometry component" (9x)

oldfac <- st_make_valid(oldfac) # Works

oldfac_poly <- oldfac %>%
  filter(st_geometry_type(.)
%in% c("POLYGON"))

oldfac_lines <- oldfac %>%
  filter(st_geometry_type(.)
         %in% c("LINESTRING"))

# Plot


plot(st_geometry(oldfac))

# oldfac - polygons

tm_shape(ind_fac) +  # Polygons from oldfac are redundant
  tm_polygons(col = "blue") + 
  tm_shape(oldfac_poly) + 
  tm_polygons(col = "red") 

# oldfac - lines

tm_shape(ind_pipes) + 
  tm_lines(col = "blue") +
  tm_shape(oldfac_lines) + 
  tm_lines(col = "red")