############################################################################
#########   DETERMINE USED AND AVAILABLE POINTS FOR CORRIDOR RSF ###########
############################################################################

library(sf)
library(dplyr)

rm(list = ls())

# ---------- LOAD DATA  ------------------------ #

corr <- readRDS('./Data/Derived-data/corridor_data.Rds')

corr <- corr %>%
  st_as_sf() %>%
  st_set_crs(3338) 

corr <- filter(corr, case_ == "TRUE") # only include used points

corr <- as_tibble(corr) # see what class the values are

corr$dist_to_coast <- as.numeric(corr$dist_to_coast)

## Values for mainland points (because I only have values for mainland points in corr dataframe)

max(corr$dist_to_coast, na.rm = TRUE) # 27.2 km
mean(corr$dist_to_coast, na.rm = TRUE) # 2 km  
sd(corr$dist_to_coast, na.rm = TRUE)

# --------  Calculate distance to coast for water points ------------------------- #

corr <- corr %>%
  mutate(dist_to_coast = st_distance(., coast)) # will not work without . 

water <- corr %>%
  mutate(dist_to_coast = ifelse(in_water == 0, NA_character_, dist_to_coast))

water$dist_to_coast <- as.numeric(water$dist_to_coast)

## Values for water points

max(water$dist_to_coast, na.rm = TRUE) # 58 km
mean(water$dist_to_coast, na.rm = TRUE) # 11 km
sd(water$dist_to_coast, na.rm = TRUE) # 16 km 

# --------  Plot  --------------------------------------------- #

water <- filter(corr, in_water == 1)
sample_water <- slice_sample(water, prop = .01, replace = FALSE) # randomly select 1% of observations

## Plot to check that distance fxn worked correctly

tmap_mode('view')

tm_shape(coast) + # Looks OK but did not verify using Measure tool in ArcGIS
  tm_lines(col = "green") + 
  tm_shape(sample_water) + 
  tm_symbols(col = popup.vars = 'dist_to_coast')

## Plot to show George and Stewart what location distribution looks like

main <- filter(corr, in_water == 0 & on_island == 0)

tmap_mode('view')

tm_shape(coast) + 
  tm_lines(col = "#875AC1") +
  tm_shape(water) + 
  tm_symbols(col = "#2467C2") + 
  tm_shape(main) + 
  tm_symbols(col = "#49491A")

# ------ OTHER NUMBERS  ------------------------------- #
  
# How many points are in water vs. mainland vs. island:

sum(corr$in_water) # 502 (18%)
sum(corr$on_island) # 1510 (54%)
# mainland = 778 (28%)

unique(corr$id)
  
  
  