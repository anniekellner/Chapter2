####################################
###   Study Extent  ################
####################################

# Create bounding box of study area
# Remove points east of US-CA border

# Only 46 points total removed across all bear-summers

library(sf)
library(tmap)
library(tmaptools)
library(dplyr)
library(here)

rm(list = ls())

# ---- LOAD DATA ------ #

pb <- readRDS(here("Data", "Derived-data", "DFs", "OG", "OG_082823.Rds")) # dataframe

# ----  REMOVE POINTS WEST OF AK-CA BORDER  ----- #

pb2 <- filter(pb, gps_lon < -141) # 141st Meridian

pbsf <- st_as_sf(pb2, coords = c("gps_lon", "gps_lat"), crs = 4326)

pbsf <- cbind(st_coordinates(pbsf), pbsf) 

# Check via plot 

tmap_mode('view')

tm_shape(pbsf) + 
  tm_symbols(popup.vars = TRUE)

bb <- st_as_sfc(st_bbox(pbsf))

# Save bounding box
st_write(bb, here("Data", "Spatial", "Derived", "Bounding_boxes", "West_of_141.shp"))

pb3 <- st_drop_geometry(pbsf) # save as df

# Rename X and Y to lat/lon so can add Albers when needed

pb3 <- pb3 %>%
  rename(gps_lon = X) %>%
  rename(gps_lat = Y)

# When removed points in Canada, some bears lost study_end dates. Add back to last point on land in USA.

end <- pb3 %>%
  group_by(id) %>%
  slice_tail() %>%
  mutate(study_end = 1)

saveRDS(pb3, here("Data", "Derived-data", "DFs", "OG", "OG_083023.Rds"))
