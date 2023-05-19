##########################################
##    EXPLORE BP ENERGY LAYERS   #########
##########################################

# Spatial layers from the BP_GIS folder

library(sf)
library(tmap)
library(tmaptools)

rm(list = ls())

# Directories

setwd('C:/Users/akell/Documents/ArcGIS/Polar_Bears_GIS/GIS_from_Todd/Industry_GIS') #  When working from laptop


###### ----------------- BP_GIS ----------------------- ###########################

#### ENDICOTT

endicott <- st_read('./OIL/BP_GIS/infrastructure_eofendicott.shp')

tm_shape(endicott) +
  tm_polygons()

## endicott looks like a bunch of different types of infrastructure - pads, etc

#### SAND
# polylines that look like rivers extending north-south

sand <- st_read('./OIL/BP_GIS/sand_poly') # Do not know what this is
plot(st_geometry(sand))

#### PADIN 
## Interiorsof well pads (look like pipelines)

padin <- st_read('./OIL/BP_GIS/padin_poly')
plot(st_geometry(padin))

tm_shape(padin) + 
  tm_lines()

#### PADEX
## Exterior of well pads (outlines)
padex <-st_read('./OIL/BP_GIS/padex_poly')
plot(st_geometry(padex))

# Are padin and padex the same?
# Padex = exterior, padin = interior

tm_shape(padin) + 
  tm_lines(col = "green") +
  tm_shape(padex) +
  tm_lines(col = "blue")

#### MUD
## Mud pits along coast 

mud <- st_read('./OIL/BP_GIS/mud_poly')
plot(st_geometry(mud))

mud_arc <- st_read('./OIL/BP_GIS/mud_arc')
plot(st_geometry(mud_arc))

tm_shape(mud) + 
  tm_lines(col = "brown") +
  tm_shape(mud_arc) +
  tm_lines(col = "red")

## Ask Todd about mud

#### MINEAIR
## No idea what this is - ask
mineair <- st_read('./OIL/BP_GIS/mineair_poly')
plot(st_geometry(mineair))

### MASK ARC

mask_arc <- st_read('./OIL/BP_GIS/mask_arc')
plot(st_geometry(mask_arc))

#### INF_PNT AND INF_ARC
## Looks like crazy dense infrastructure
inf_pnt <- st_read('./OIL/BP_GIS/inf_pnt')
plot(st_geometry(inf_pnt))
plot(inf_pnt)

inf_arc <- st_read('./OIL/BP_GIS/inf_arc')
plot(st_geometry(inf_arc))


tm_shape(inf_pnt) + 
  tm_symbols() +
  tm_shape(inf_arc) + 
  tm_lines()

## HYD

hyd_poly <- st_read('./OIL/BP_GIS/hyd_poly')
plot(hyd_poly)

hyd_arc <- st_read('./OIL/BP_GIS/hyd_arc')

tm_shape(hyd_poly) +
  tm_lines() +
  tm_shape(hyd_arc) +
  tm_lines()





