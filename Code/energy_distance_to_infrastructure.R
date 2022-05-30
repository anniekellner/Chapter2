################################################
#####   ENERGY: DISTANCE TO INFRASTRUCTURE  ####
################################################

# Combine all energy infrastructure into a single entity and calculate distance to it

# Register with google API so can make maps
#register_google(key = "AIzaSyDSOPDGRNBNE-ZoLj4PM608-dpDrQ0VNgg", write = TRUE)

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap) # square maps - a little annoying in a place where rectangluar would be better

rm(list = ls())

# --  CONOCO-PHILLIPS ----------------------- #

# Read in all .shp files from a directory

cp <- list.files(path = './Data/Spatial/Industry_GIS/CP_Infrastructure', pattern = "[.]shp$", full.names = TRUE)

cp_shp <- list()

for(i in 1:length(cp)){
 shp = st_read(cp[i])
 shp = st_transform(shp, 3338)
cp_shp[[i]] = shp
}

cp2 <- bind_rows(cp_shp) # will not save to .shp - throws error

cp3 <- cp2 %>%
  drop_na(geometry)

#plot(st_geometry(cp2))

# -- HILCORP ------------------------------------------- #

hil <- list.files(path = './Data/Spatial/Industry_GIS/Hilcorp', pattern = "[.]shp$", full.names = TRUE)

hil_shp <- list()

for(i in 1:length(hil)){
  shp = st_read(hil[i])
  shp = st_transform(shp, 3338)
  hil_shp[[i]] = shp
}

hil2 <- bind_rows(hil_shp)
#plot(st_geometry(hil2))


# ----- OTHER ------------------------------------- #

fac <- st_read('./Data/Spatial/Industry_GIS/Facilities/Faciliites.shp')
fac <- st_transform(fac, 3338)

fac2 <- st_read('./Data/Spatial/Industry_GIS/OIL/FAC.shp')
fac2 <- st_transform(fac2, 3338)

fac3 <- st_read('./Data/Spatial/Industry_GIS/OIL/FAC2_3.shp')
fac3 <- st_transform(fac3, 3338)

pipes <- st_read('./Data/Spatial/Industry_GIS/OIL/PIPES.shp')
pipes <- st_transform(pipes, 3338)

prop_fac <- st_read('./Data/Spatial/Industry_GIS/OIL/PROP_FAC.shp')
prop_fac <- st_transform(prop_fac, 3338)

endicott <- st_read('./Data/Spatial/Industry_GIS/OIL/BP_GIS/infrastructure_eofendicott.shp')
endicott <- st_transform(endicott, 3338)

other_shp <- bind_rows(fac,fac2,fac3,pipes,prop_fac,endicott)

#--- PLOT -------------------------------------------- #

# Put all data together and ask Todd about 'other'

# Get bbox so can crop map

st_bbox(cp3)
st_bbox(hil2)
st_bbox(other_shp)

# Alaska

ak_map <- get_map(location = c(lon = -148.4, lat = 70.2269), zoom = 7, maptype = 'terrain-background', source = 'stamen')

ggmap(ak_map)

ggmap(ak_map) +
  geom_sf(data = cp3, color = "red", inherit.aes = FALSE) + 
  geom_sf(data = hil2, color = "blue", inherit.aes = FALSE) + 
  geom_sf(data = other_shp, color = "orange", inherit.aes = FALSE) +
  coord_sf(crs = st_crs(4326))


