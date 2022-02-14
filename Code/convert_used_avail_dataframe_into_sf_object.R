####################################################
###   CONVERT USED/AVAIL DATAFRAME INTO SF OBJECT ##
####################################################

library(dplyr)
library(amt)

rm(list = ls())


pts <- readRDS('./Data/Derived-data/non_bp_pts_used_avail.Rds') # reads in as steps dataframe. x1/y1 are the first part of the step, x2/y2 are the second.

pts <- pts %>%
  select(id, x2_, y2_, t2_, case_) %>%
  rename(c(x = x2_, y = y2_, t = t2_)) 

tr <- make_track(pts, .x = x, .y = y, .t = t, case = case_) # Make track in amt

sf <- as_sf_points(tr) # Convert to sf

saveRDS(sf, './Data/Derived-data/non_bp_ua_sf.Rds')

