#####################################################################
##    ADD POINT ID NUMBERS TO CROSS-REFERENCE FOR JOINS #############
#####################################################################

# Add point ID values to all used/avail points so can easily cross-reference dataframe when joining data

library(dplyr)

rm(list = ls())

# Bonepile

bone <- readRDS('./Data/Derived-data/bonepile_data_used_avail.Rds')

bone <- bone %>%
  mutate(Point_ID = row_number())

saveRDS(bone, './Data/Derived-data/bonepile_data.Rds')

# Corridor

corr <- readRDS('./Data/Derived-data/NB_UA_data.Rds')

corr <- corr %>%
  mutate(Point_ID = row_number())

saveRDS(corr, './Data/Derived-data/corridor_data.Rds')
