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

islands <- st_read('./Data/Spatial/Barrier_Islands/all_islands.shp')

# -- Assign Water ---------------- #

corr2 <- corr %>%
  mutate(in_water = ifelse(on_island == "FALSE" & is.na(veg) & elevation == 0 | is.na(elevation),1,0))

bone2 <- bone %>%
  mutate(in_water = ifelse(on_island == "FALSE" & elevation == 0 | is.na(elevation),1,0))

# Remove previous on_island designation and replace with new one (that uses island buffer) - 4/12/22

corr3 <- corr2 %>%
  select(-on_island) %>%
  rename(on_island = on_island.1)

bone3 <- bone2 %>%
  select(-on_island) %>%
  rename(on_island = on_island.1)


# -- Check by plotting  ----------- #

# Randomly sample 5% of rows and make sf object to plot

corr_samp <- corr3 %>%
  sample_frac(size = .05, replace = FALSE) %>%
  st_as_sf()

bone_samp <- bone3 %>%
  sample_frac(size = .05, replace = FALSE) %>%
  st_as_sf(., coords = c('x_', 'y_'), crs = 3338)

# Plot

tmap_mode('view')

tm_shape(islands) + 
  tm_polygons(col = "green") +
tm_shape(corr_samp) + 
  tm_symbols(size = 0.1, popup.vars = c('in_water')) # Looks good

tm_shape(bone_samp) + 
  tm_symbols(size = 0.1, popup.vars = c('in_water')) # Looks good



# Save

saveRDS(corr3, './Data/Derived-data/corridor_data.Rds')
saveRDS(bone3, './Data/Derived-data/bonepile_data.Rds')

