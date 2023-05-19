###########################################################################################
##    ADD TRACK DATA BACK INTO DATAFRAME   ################################################
###########################################################################################

library(dplyr)


rm(list = ls())


# ---- Load data  ------------------------------------------------- #

amt_data <- readRDS('./Data/Derived-data/non_bp_pts_used_avail.Rds')
corr <- readRDS('./Data/Derived-data/corridor_data.Rds')

# --- Merge ------------------------------------------------------ #

# Turn amt data into sf object

amt_sf <- st_as_sf(amt_data, coords = c('x2_', 'y2_'), crs = 3338)

corr <- corr %>% # rename so can join by t2_
  rename(t2_ = t_) %>%
  rename(case_ = case)


corr2 <- corr %>%
  left_join(amt_sf, by = c("id", "geometry", "t2_", "case_")) # Joined perfectly

# --- Save  -------------------------------------------------------- #

saveRDS(corr2, './Data/Derived-data/corridor_data.Rds')


