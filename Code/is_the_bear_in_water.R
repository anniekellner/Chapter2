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

uaSF <- readRDS(here("Data", "Derived-data", "DFs", "OG", "uaSF_091823.Rds"))
islands <- st_read(here("Data", "Spatial", "Barrier_Islands", "islands_w_1500m_buffer.shp"))

#   ----    CHECK NA'S    ----------    #

nas <- filter(uaSF, is.na(elevation))

tmap_mode('view')

tm_shape(nas) + 
  tm_symbols()

# -- Assign Water ---------------- #

uaSF <- uaSF %>%
  mutate(in_water = ifelse(on_island == 0 & elevation > 0, 0, 1))

bone2 <- bone %>%
  mutate(in_water = ifelse(on_island == "TRUE" | elevation > 0, 0, 1))


# -- Check  ----------- #

# Randomly sample 5% of rows and make sf object to plot

corr_samp <- corr2 %>%
  sample_frac(size = .05, replace = FALSE) %>%
  st_as_sf()

bone_samp <- bone2 %>%
  sample_frac(size = .05, replace = FALSE) %>%
  st_as_sf(., coords = c('x_', 'y_'), crs = 3338)

# Plot
# Looks good! 5-12-2022

tmap_mode('view')

tm_shape(islands) + 
  tm_polygons(col = "green") +
tm_shape(corr_samp) + 
  tm_symbols(col = "in_water", popup.vars = c("elevation", "veg", "aspect", "on_island")) 

tm_shape(islands) + 
  tm_polygons(col = "green") +
  tm_shape(bone_samp) + 
  tm_symbols(col = "in_water") 


# Save

saveRDS(corr2, './Data/Derived-data/corridor_data.Rds')
saveRDS(bone2, './Data/Derived-data/bonepile_data.Rds')

