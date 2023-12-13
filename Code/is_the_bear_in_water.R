#########################################################
##    ADD WATER TO DATAFRAMES   #########################
#########################################################

# Water is defined as elevation = 0, on_island= FALSE, veg = NA
# In looking at ifSAR, land appears to be >0
# In some cases, land = 0, but the point should register as a veg type or on island. I expect false positives to be very low.

library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library(here)
library(conflicted)

conflicts_prefer(
  dplyr::filter()
)

rm(list = ls())

# --- Load Data -------------------- #

uaSF <- readRDS(here("Data", "Derived-data", "DFs", "OG", "uaSF_12-12-23.Rds"))
islands <- st_read(here("Data", "Spatial", "Barrier_Islands", "islands_w_1500m_buffer.shp"))

#   ----    CHECK NA'S    ----------    #

nas <- filter(uaSF, is.na(elevation)) # in GEE, I replaced values less than 0 with 0 (ie nearshore points) but did not account for NA pts

tmap_mode('view')

tm_shape(nas) + 
  tm_symbols()

# Replace NA's with 0

uaSF <- uaSF %>%
  replace_na(list(elevation = 0))


# -- Assign Water ---------------- #

uaSF <- uaSF %>%
  mutate(in_water = ifelse(on_island == 0 & elevation > 0, 0, 1))

# -- Check  ----------- #

# Randomly sample 1% of rows and make sf object to plot

samp <- slice_sample(uaSF, prop = 0.01)

tmap_mode('view')

tm_shape(islands) + 
  tm_polygons(fill = "green") +
tm_shape(samp) + 
  tm_symbols(col = "in_water", popup.vars = c("elevation", "aspect", "on_island")) 

# Save

saveRDS(corr2, './Data/Derived-data/corridor_data.Rds')
saveRDS(bone2, './Data/Derived-data/bonepile_data.Rds')

