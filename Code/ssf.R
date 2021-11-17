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
ggplot(ssf1, aes(x2_, y2_, color=case_))+
  geom_point()+
  facet_wrap(~id, scales="free")

# Convert back to sf object so can match up with spatial covariate data

ssf1_sf <- st_as_sf(ssf1, coords = c('x2_', 'y2_'), crs = 3338)

# Write to csv for use with GEE

#ssfi1_gee <- select(ssf1, id, x2_, y2_, case_)
#write.csv(ssf1, file = 'C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Chapter2/Data/GEE/ssf_1hr.csv')

# Write to Data folder

saveRDS(ssf1_sf, file = './Data/ssf_1hr_sf.Rds')


