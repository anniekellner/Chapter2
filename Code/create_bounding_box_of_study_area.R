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
pb <- select(pb, -study_end)

# ----  REMOVE POINTS WEST OF AK-CA BORDER  ----- #

pb2 <- filter(pb, gps_lon < -141) # 141st Meridian

# When removed points in Canada, some bears lost study_end dates. Add back to last point on land in USA.

end <- pb2 %>% # skipped pbsf so if 
  group_by(id) %>%
  slice_tail() %>%
  mutate(study_end = 1) %>%
  select(id, datetime, study_end)

pb4 <- left_join(pb3, end)
filter(pb4, study_end == 1)

saveRDS(pb3, here("Data", "Derived-data", "DFs", "OG", "OG_083023.Rds"))
