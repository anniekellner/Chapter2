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

# Covariates

mean(steps1$sl_) # calculate mean step length for buffer. 500m should work 

veg <- st_read('D:/Polar Bears/Data/Chapter2/veg/aga_arctic_ak_geobotanical_shp/aga_arctic_ak_geobotanical.shp')
veg_aa <- st_transform(veg, 3338)

steps1 <- steps[[1]]

ssf1 <- steps1 %>% group_by(id) %>% random_steps(n_control = 15)

# Plot random v matched points

ggplot(ssf1, aes(x2_, y2_, color=case_))+geom_point()+facet_wrap(~id, scales="free") # plot of random v. matched points

# Convert back to sf object so can match up with spatial covariate data

ssf1_sf <- st_as_sf(ssf1, coords = c('x2_', 'y2_'), crs = 3338)

test <- st_intersection(ssf1_sf, veg_aa)
t

m0 <- test %>% fit_clogit(case_ ~ COMM + strata(step_id_))
summary(m0)
