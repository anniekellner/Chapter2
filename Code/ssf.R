###################################################
###   FITTING A STEP-SELECTION FUNCTION WITH AMT  #
###################################################

# https://cran.r-project.org/web/packages/amt/vignettes/p4_SSF.html

library(lubridate)
library(raster)
library(amt)
library(sf)
library(ggplot2)

rm(list = ls())

# Data - list of dataframes in which each list item is a dataframe grouped by fix rate (1,2,4,8)

steps <- readRDS('./Data/Derived-data/step_lengths.Rds') # step length data from Step_length.R

steps1 <- steps[[1]] # subset one hr fix rate to start (do other rates later)
ssf1 <- steps1 %>% group_by(id) %>% random_steps(n_control = 15) # add random steps

# Plot random v matched points
ggplot(ssf1, aes(x2_, y2_, color=case_))+geom_point()+facet_wrap(~id, scales="free") # plot of random v. matched points

# Convert back to sf object so can match up with spatial covariate data

ssf1_sf <- st_as_sf(ssf1, coords = c('x2_', 'y2_'), crs = 3338)

# Write to csv for use with GEE

#ssfi1_gee <- select(ssf1, id, x2_, y2_, case_)
#write.csv(ssf1, file = 'C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Chapter2/Data/GEE/ssf_1hr.csv')

# ----------------- Bonepile ----------------------------------------------------------------- #

bone <- st_read('./Data/Spatial/Bonepiles/bonepiles.shp') 
bone <- st_transform(bone, 3338)

for(i in 1:length(ssf1_sf)){
  ssf1$dist_bonepile[i] = st_distance(ssf1_sf[i], bone, by_element = TRUE)
}

ssf1_covs <- ssf1_sf %>%
  mutate(dist2bonepile = st_distance(ssf1_sf, bone), by_element = TRUE)

mean(steps1$sl_) # calculate mean step length for buffer. 500m should work 

veg <- st_read('D:/Polar Bears/Data/Chapter2/veg/aga_arctic_ak_geobotanical_shp/aga_arctic_ak_geobotanical.shp')
veg_aa <- st_transform(veg, 3338)





test <- st_intersection(ssf1_sf, veg_aa)
t

m0 <- test %>% fit_clogit(case_ ~ COMM + strata(step_id_))
summary(m0)
