############################################################
######    CONVERT USED AND AVAILABLE DATAFRAMES TO SHP'S  ##
######      FOR USE WITH GOOGLE EARTH ENGINE  ##############
############################################################

# 12-20-23
# All used and available points in a single df
# Used with GEE to extract terrain data


library(sf)
library(tidyverse)
library(here)
library(conflicted)

rm(list = ls())

#   --------  LOAD DATA   ---------------   #

ua <- readRDS(here("Data", "Derived-Data", "DFs", "OG", "uaSF_12-15-23.Rds"))
ua <- ua %>%
  bind_cols(st_coordinates(ua)) 

ua <- ua %>%
  rename(Xaa = X) %>%
  rename(Yaa = Y)

ua <- ua %>%
  arrange(id, t2_) %>%
  mutate(row = row_number()) 

#saveRDS(ua, here("Data", "Derived-Data", "DFs", "OG", "uaSF_12-20-23.Rds"))
#   --------  WRITE TO SHP    ------------    #

st_write(ua, here("Data", "Derived-data", "Spatial", "UA_shapefiles", "uaSHP_12-20-23.shp")) # got warning that t2_ created as Date field, but DateTime requested

