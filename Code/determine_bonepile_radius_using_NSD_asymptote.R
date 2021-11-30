##############################################################################################
###   USE NSD TO DETERMINE RADIUS AROUND BONEPILE FOR DIVIDING BEARS INTO GROUPS    ##########
##############################################################################################

library(adehabitatLT)
library(sf)
library(dplyr)

rm(list = ls())

# ----------------------- Create traj object  ----------------------------------------- #

source('./Code/MyFunctions.R') # for st_drop_geometry

# Load data and create traj object

pb <- readRDS('./Data/bears_092921.Rds')

pb <- cbind(pb, st_coordinates(pb)) # separate coords from geometry columns into X and Y columns

pbdf <- as.data.frame(pb)


# --------------------  Create dataset that is 'asymptote' around bonepile    ---------- #

# Bears with all points at bonepile (reference: bonepile_denning_info.xlsx)

bp_only <- pbdf %>%
  filter(id == "pb_20525.2013" | id == "pb_20525.2014" | id == "pb_20586.2008" | id == "pb_32366.2014") %>%

# Bears that travel to bonepile

pb06810 <- pbdf %>%
  filter(id == "pb_06810.2008") %>%
  filter(date > "2008-09-16 18:00:37")

pb20966 <- pbdf %>%
  filter(id == "pb_20966.2008") %>%
  filter(date > "2008-08-26 16:00:00")

pb20492 <- pbdf %>%
  filter(id == "pb_20492.2008") %>%
  filter(date < "2008-10-16")

pb20520 <- pbdf %>% 
  filter(id == "pb_20520.2012") %>%
  filter(date > "2012-08-27 20:00:00")

pb20735 <- pbdf %>%
  filter(id == "pb_20735.2009") %>%
  filter(date > "2009-08-09 22:00:00" & date < "2009-08-29 21:00:00" | date > "2009-09-16 01:00:00")

pb20845 <- pbdf %>%
  filter(id == "pb_20845.2015") %>%
  filter(date > "2015-09-23 20:01:29" & date < "2015-10-02 15:00:09")

pb21015 <- pbdf %>%
  filter(id == "pb_21015.2013") %>%
  filter(date > "2013-08-20 08:00:31" & date < "2013-09-27 04:00")

pb21368 <- pbdf %>%
  filter(id == "pb_21368.2014") %>%
  filter(date > "2014-08-26 04:00:00") 

pb32282 <- pbdf %>%
  filter(id == "pb_32282.2008") %>%
  filter(date > "2008-08-31 16:00:00")

pb32366_2011 <- pbdf %>%
  filter(id == "pb_32366.2011") %>%
  filter(date > "2011-08-30 12:00:00")

all_bp <- pbdf %>%
  filter(id == "pb_32608") %>%
  filter(date > "2008-08-30 10:00:00" & date < "2008-10-15 07:00:00") %>%
  bind_rows(bp_only, pb06810, pb20966, pb20492, pb20520, pb20735, pb20845, pb21015, pb21368, pb32282, pb32366_2011)



# Divide into groups based on median fix rate

unique(trav$median_fix)

trav1 <- filter(trav, median_fix == 1)
trav2 <- filter(trav, median_fix == 2)

sd(trav1$dist, na.rm = TRUE) *2 # 647.8
sd(trav2$dist/2, na.rm = TRUE) * 2 # 354 m 







