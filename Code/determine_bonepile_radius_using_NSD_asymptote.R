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

traj.pb<-as.ltraj(xy=pbdf[,c("X","Y")], date=pbdf$datetime, id=as.character(pbdf$id))
traj.df <- ld(traj.pb)

# --------------------  Create dataset that is 'asymptote' around bonepile    ---------- #

# Bears with all points at bonepile (reference: bonepile_denning_info.xlsx)

# Load akde table to get median fix rates

akde <- readRDS('./Data/akde_df.Rds')

bp_only <- traj.df %>%
  filter(id == "pb_20525.2013" | id == "pb_20525.2014" | id == "pb_20586.2008" | id == "pb_32366.2014") %>%
  inner_join(akde)

bp1 <- bp_only %>%
  filter(median_fix == 1)

bp2 <- bp_only %>%
  filter(median_fix == 2)

mean(sqrt(bp1$R2n)) # 425 m
sd(bp1$dist, na.rm = TRUE) * 2 # 476.7 m
mean(sqrt(bp2$R2n)/2) # 727.0
sd(bp2$dist/2, na.rm = TRUE) * 2 # 598 m

# Bears that travel to bonepile

pb06810 <- traj.df %>%
  filter(id == "pb_06810.2008") %>%
  filter(date > "2008-09-16 18:00:37")

pb20966 <- traj.df %>%
  filter(id == "pb_20966.2008") %>%
  filter(date > "2008-08-26 16:00:00")

trav <- traj.df %>%
  filter(id == "pb_21368.2014") %>%
  filter(date > "2014-08-26 04:00:00") %>%
  bind_rows(pb06810, pb20966) %>%
  inner_join(akde)

# Divide into groups based on median fix rate

unique(trav$median_fix)

trav1 <- filter(trav, median_fix == 1)
trav2 <- filter(trav, median_fix == 2)

sd(trav1$dist, na.rm = TRUE) *2 # 647.8
sd(trav2$dist/2, na.rm = TRUE) * 2 # 354 m 







