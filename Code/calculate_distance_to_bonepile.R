#####################################################################
##    DISTANCE TO BONEPILE    #######################################
#####################################################################

# Remember that not all bonepiles were there in all years

library(sf)

rm(list = ls())

# --------------------- Data ----------------------------------------------------------------- #



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