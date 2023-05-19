###########################################################################
######    RENAME LANDFALL AND ADD STUDY.START COLUMNS #####################
###########################################################################

# Landfall - Remove 'landfall' designation for 2008 bears that were collared on land
# See ice_arrive_depart.csv for details

# 21237.2011 was not collared on land, but bear is missing data between July and September. Bear appears on land in September.  
# 20525.2014 did not really swim, but walked off ice onto land.  

library(dplyr)

rm(list = ls())

# ------------- LOAD AND PREP DATA  ------------------ #

bears <- readRDS('./Data/Derived-data/DFs/bears_ch2_092122.Rds')

# ----------  ADJUST COLUMNS  ----------------- #

bears$study.start <- bears$landfall # Change what is now 'landfall' to study.start

# Reserve 'landfall' for bears that were collared at sea and came ashore

b2008 <- filter(bears, year == 2008)
b2008 <- unique(b2008$id)
b2008 <- b2008[-2] # remove pb_20333.2008 because this bear was collared on ice

bears2 <- bears %>%
  mutate(landfall = if_else(id %in% b2008, 0, landfall))

#saveRDS(bears2, './Data/derived-data/DFs/bears_ch2_093022.Rds') # save with 'landfall' and 'study.start' columns changed

