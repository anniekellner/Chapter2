############################################################
######    CONVERT USED AND AVAILABLE DATAFRAMES TO SHP'S  ##
######      FOR USE WITH GOOGLE EARTH ENGINE  ##############
############################################################

# 09-14-23
# All used and available points in a single df
# Used with GEE to extract terrain data


library(sf)
library(tidyverse)
library(here)
library(conflicted)

#   --------  LOAD DATA   ---------------   #

ua <- readRDS(here("Data", "Derived-Data", "DFs", "OG", "uaSF.Rds"))

#   --------  WRITE TO SHP    ------------    #

#st_write(ua, here("Data", "Derived-data", "Spatial", "UA_shapefiles", "uaSHP.shp")) 

