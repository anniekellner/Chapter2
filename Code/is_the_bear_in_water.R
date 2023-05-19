#########################################################
##    ADD WATER TO DATAFRAMES   #########################
#########################################################

# Water is defined as elevation = 0, on_island= FALSE, veg = NA
# In looking at ifSAR, land appears to be >0
# In some cases, land = 0, but the point should register as a veg type or on island. I expect false positives to be very low.

library(sf)
library(dplyr)
library(tmap)
library(tmaptools)

rm(list = ls())

# --- Load Data -------------------- #

corr <- readRDS('./Data/Derived-data/corridor_data.Rds')

bone <- readRDS('./Data/Derived-data/bonepile_data.Rds')

islands <- st_read('./Data/Spatial/Barrier_Islands/islands_w_1500m_buffer.shp') 


# -- Assign Water ---------------- #

corr2 <- corr %>%
  mutate(in_water = ifelse(on_island == "TRUE" | !is.na(veg) | elevation > 0, 0, 1))

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

