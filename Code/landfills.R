################################################################
####    ADD LANDFILLS   ########################################
################################################################

## Data from https://gis.data.alaska.gov/datasets/DCCED::landfills/explore?location=29.322995%2C0.000000%2C2.64&showTable=true

library(sf)
library(dplyr)

rm(list = ls())

# -------   LOAD DATA ------------------------------- #

lf <- read.csv('C:/Users/akell/OneDrive - Colostate/Documents/Education/PhD/Chapter2/Data/Landfills.csv') %>%
  select(Site.Name, Classification, Location, Site.Latitude, Site.Longitude)

lf.sf <- st_as_sf(lf, coords = c("Site.Longitude", "Site.Latitude")) %>%
  st_set_crs(4326) %>% st_transform(3338)

saveRDS(lf.sf, './Data/Spatial/Municipal/landfills.Rds')
