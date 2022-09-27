##################################################
########    NSD PLOTS FOR SUPPINFO    ############
##################################################

# Create fig for SuppInfo that shows NSD plots for all bears

library(dplyr)
library(adehabitatLT)
library(sf)
library(ggplot2)
library(scales)
library(tmap)
library(tmaptools)


rm(list = ls())

# -------------  DATA PREP  ------------------  #

pb <- readRDS('./Data/Derived-data/DFs/bears_ch2_092122.Rds') # loads as df
pb <- st_as_sf(pb)

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
    id == "pb_32255.2008" | id == "pb_21237.2011" | id == "pb_20418.2005" | 
      id == "pb_20414.2009" ~ "coast",
    TRUE ~ "coast_and_bonepile"
  ))

# Add column for which bonepile

arrival <- traj.df2 %>%
  group_by(id) %>%
  slice_head() %>%
  filter(bear_type == "bonepile_only" | bear_type == "coast_and_bonepile") %>%
  mutate(which_bonepile = if_else(
  id == "pb_20492.2008" | id == "pb_20520.2012" | id == "pb_20735.2009" | id == "pb_20966.2008" | 
    id == "pb_20982.2008" | id == "pb_32282.2008" | id == "pb_32366.2011" | id == "pb_32608.2008",
  "Kaktovik", "Cross")) %>%
  dplyr::select(x, y, date, id, bear_type, which_bonepile) %>%
  st_as_sf(coords = c('x', 'y'), crs = 3338) %>%
  glimpse()

# Bonepiles

bonepiles <- st_read('./Data/Spatial/Bonepiles/bonepiles.shp')

# ----------- PLOTS -------------------------- #

# plot bonepiles and arrival locations to make sure they look right
# Look good 9/27/22

tmap_mode('view')

tm_shape(bonepiles) + 
  tm_dots(col = "blue", size = 0.5) + 
  tm_shape(arrival) +
  tm_dots(col = "red")

# ------------- CALCULATE DISTANCE  ----------------- #



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


