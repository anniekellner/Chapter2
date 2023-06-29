#################################################################
###     CREATE CURRENT ENERGY LAYERS    #########################
#################################################################

# It is much easier to compare layers in ArcGIS over R


library(tidyverse)
library(sf)
library(maps)
library(googleway) # can pull geographic coords from Google
theme_set(theme_bw())
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(conflicted)

rm(list = ls())

# --------- LOAD DATA ---------------------------------------------- #

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

other_ind <- st_union(fac, end)
  
fac2 <- st_read('C:/Users/akell/OneDrive - Colostate/Documents/ArcGIS/GIS from Todd/Industry GIS/OIL/FAC.shp') %>% 
  st_transform(3338) %>%
  select(geometry) %>%
  mutate(layer = "fac2_3")

other_ind <- st_union(other_ind, fac2)

pipes <- st_read('C:/Users/akell/OneDrive - Colostate/Documents/ArcGIS/GIS from Todd/Industry GIS/OIL/PIPES.shp') %>%
  st_transform(3338) %>%
  select(geometry) %>%
  mutate(layer = "pipes")

other_ind <- st_union(other_ind, pipes)

prop_fac <- st_read('C:/Users/akell/OneDrive - Colostate/Documents/ArcGIS/GIS from Todd/Industry GIS/OIL/PROP_FAC.shp') %>%
  st_transform(3338) %>%
  select(geometry) %>%
  mutate(layer = "PROP_FAC")

other_ind <- st_union(other_ind, prop_fac)

roads <- st_read('C:/Users/akell/OneDrive - Colostate/Documents/ArcGIS/GIS from Todd/Industry GIS/OIL/ROADS.shp') %>%
  st_transform(3338) %>%
  select(geometry) %>%
  mutate(layer = "roads")

other_ind <- st_union(other_ind, roads)

tiger_rds <- st_read('C:/Users/akell/OneDrive - Colostate/Documents/ArcGIS/GIS from Todd/Industry GIS/OIL/tiger_roads.shp') %>%
  st_set_crs(4326) %>% st_transform(3338) %>%
  select(geometry) %>%
  mutate(layer= "tiger roads")

other_ind <- st_union(other_ind, tiger_rds)

# Plot

plot(st_geometry(other_ind))

# Other facilities



fac2_3 <- st_read('C:/Users/akell/OneDrive - Colostate/Documents/ArcGIS/GIS from Todd/Industry GIS/OIL/FAC2_3.shp')
fac23 <- st_transform(fac2_3, 3338)

transak <- st_read('./Data/Spatial/Industry_GIS/North Slope Science/trans_alaska_pipeline/Transportation - Pipelines - Trans Alaska Pipeline System_LINE.shp')






# ------------ CHECK AGAINST TODD EMAIL --------------------------------------- #

## Compare NSSI to CP/Hilcorp Industry data

# Combine CP and Hilcorp pipes/roads

cp$OPERATOR <- "CP"

cp <- cp %>%
  select(NAME, OPERATOR, Type, LABEL, geometry) %>%
  rename(TYPE = Type)

hil <- hil %>%
  select(NAME, OPERATOR, TYPE, LABEL, geometry)

hil$TYPE <- as.character(hil$TYPE) # Change from integer to character in order to use bind_rows()

ind <- bind_rows(cp, hil)

# Get intersection

inter <- st_intersection(ind, ns, tolerance = 1000) # Looked at distance between Hilcorp shp and NS shp in Arcmap. Very rough estimate. 

plot(st_geometry(inter))
plot(st_geometry(ns), col = "red", add = TRUE) # Can manually assess differences because there are not too many. 

# Overlay NS facilities with CP/Hilcorp facilities

cpfac$OPERATOR <- "CP"

cpfac <- cpfac %>%
  select(NAME, OPERATOR, geometry) 

# Hilcorp

hil_fac <- hil_fac %>%
  select(NAME, OPERATOR, geometry)

indfac <- bind_rows(cpfac, hil_fac)

interfac <- st_intersection(indfac, nsfac)

# ----- SEPARATE RESIDENTIAL FROM INDUSTRIAL AREAS IN NSSI DATA  ------------------------------ #

## Developed areas

unique(diffac$Type2)

residential <- filter(diffac, Type2 == "Townsite" | Type2 == "Homestead")

#saveRDS(residential, './Data/Derived-data/Spatial/NSSI/NSDev_residential.Rds')

ns_industrial <- diffac %>%
  filter(!Type2 %in% c('Townsite', 'Homestead'))

#saveRDS(ns_industrial, './Data/Derived-data/Spatial/NSSI/NSDev_industrial.Rds')

nsind2 <- st_zm(nsind, drop=T, what='ZM') # https://community.rstudio.com/t/issues-with-writing-a-dataframe-to-a-shapefile/42449/6

#st_write(nsind2, './Data/Derived-data/Spatial/NSSI/NSDev_industrial.shp') # Works fine when Z-dimension is removed

tmap_mode('view')

tm_shape(residential) + # Looks good
  tm_symbols()

tm_shape(ns_industrial) + 
  tm_symbols()


# ------  PLOT TO SEE DIFFERENCES BTW CP/HILCORP AND NSSI   ------------------------------ #

sf_use_s2(FALSE) # Otherwise get errors; https://github.com/r-spatial/sf/issues/1856

## Background maps

world <- ne_countries(scale = "medium", returnclass = "sf") # rnaturalearth package
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

# Get city coords from Google

key <- "AIzaSyDSOPDGRNBNE-ZoLj4PM608-dpDrQ0VNgg" # Google API key

akcities <- data.frame(state= rep("Alaska", 3), city = c("Niuqsut", "Deadhorse", "Kaktovik"))

coords <- apply(akcities, 1, function(x){
  google_geocode(address = paste(x["city"], x["state"], sep = ", "), 
                 key = key)
})

akcities <- cbind(akcities, do.call(rbind, lapply(coords, geocode_coordinates)))

akcities <- st_as_sf(akcities, coords = c("lng", "lat"), remove = FALSE, # Convert to SF format
                     crs = 4326, agr = "constant")

# Oil and gas data
# Not sure this works or is necessary...

cp2 <- st_as_sf(cp, coords = c("longitude", "latitude"), # Needs to be in lat/long to be compatible with ggplot features
                crs = 4326, agr = "constant")

hil2 <- st_as_sf(hil, coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")

faa <- st_as_sf(faa, coords = c("longitude", "latitude"), 
                crs = 4326, agr = "constant")

ns2 <- ns %>% 
  st_as_sf(ns, coords = c("longitude", "latitude"), 
               crs = 4326, agr = "constant")

## ggplot

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = akcities) +
  geom_text_repel(data= akcities, aes(x = lng, y = lat, label = city),
            size = 3.0, fontface = "bold", nudge_y = c(-0.25, -0.25, 0.25)) + # package ggrepel = flexible approach to label placement
  geom_sf(data = inter, color = "red") +
  geom_sf(data = ns, color = "blue") +
  coord_sf(xlim = c(-152, -143), ylim = c(70.0,70.8), expand = FALSE)



plot(st_geometry(interfac), col = "red")
plot(st_geometry(nsfac), col = "blue", add = TRUE)

# Save intersection .shp for use with ArcGIS

#st_write(interfac, 'C:/Users/akell/Documents/ArcGIS/Projects/Chapter2/Energy/North Slope Science/Derived-data/inter_fac.shp')
st_write(interfac, 'C:/Users/akell/Documents/ArcGIS/Projects/Chapter2/Energy/North Slope Science/Derived-data/inter_fac.shp')



ggplot(data = world) +
  geom_sf() +
  geom_sf(data = akcities) +
  geom_text_repel(data= akcities, aes(x = lng, y = lat, label = city),
                  size = 3.0, fontface = "bold", nudge_y = c(-0.25, -0.25, 0.25)) + # package ggrepel = flexible approach to label placement
  geom_sf(data = cp2, color = "red") +
  geom_sf(data = hil2, color = "red") +
  geom_sf(data = faa, color = "blue") +
  geom_sf(data = ns, color = "green") +
  geom_sf(data = iceroad, color = "#C5E9F6") +
  geom_sf(data = npra_icerd, color = "#C5E9F6") +
  coord_sf(xlim = c(-152, -143), ylim = c(70.0,70.8), expand = FALSE)


tmap_mode('view')
tm_shape(iceroad) + 
  tm_lines()

ns %>%
  filter(str_detect(Route_Name, 'thom')) 
