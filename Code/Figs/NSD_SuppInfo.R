##################################################
########    NSD PLOTS FOR SUPPINFO    ############
##################################################

# Create fig for SuppInfo that shows NSD plots for all bears

library(dplyr)
library(adehabitatLT)
library(sf)
library(ggplot2)
library(scales)


rm(list = ls())

# -------------  DATA PREP  ------------------  #

pb <- readRDS('./Data/bears_092921.Rds')

# Create traj df

pb <- cbind(pb, st_coordinates(pb)) # separate coords from geometry columns into X and Y columns

pbdf <- as.data.frame(pb)

traj.pb<-as.ltraj(xy=pbdf[,c("X","Y")], date=pbdf$datetime, id=as.character(pbdf$id))
traj.df <- ld(traj.pb)

# Add bear movement category (from use_nsd_to_determine_bonepile_arrival.R)

traj.df2 <- traj.df %>%
  mutate(bear_type = case_when(
    id == "pb_20525.2013" | id == "pb_20525.2014" | id == "pb_20586.2008" | 
      id == "pb_32366.2014" ~ "bonepile_only",
    id == "pb_32255.2008" | id == "pb_pb_21237.2011" | id == "pb_20418.2005" | 
      id == "pb_20414.2009" ~ "coast",
    TRUE ~ "coast_and_bonepile"
  ))

# ----------- PLOT -------------------------- #

uni <- unique(traj.df$id)

traj.df2$R2n <- traj.df2$R2n/1000 # Change NSD scale to km 

cb <- filter(traj.df2, bear_type == "coast_and_bonepile")

boneplot <- ggplot(data = cb, aes(x = date, y = R2n)) + 
  geom_line(size = 1) +
  geom_point() + 
  xlab("\nCalendar Date") + 
  ylab("Net Squared Displacement from Arrival Point (km)\n") +
  theme_minimal() +
  facet_wrap(~id, scales = "free")

ggtemp = ggplot(data = traj.df2, aes(x = date, y = R2n)) + 
  geom_line(size = 1) +
  geom_point() + 
  xlab("\nCalendar Date") + 
  ylab("Net Squared Displacement from Arrival Point (m)\n") +
  theme_minimal() +
  facet_wrap(~id, scales = "free")


