########################################################################
#####   DETERMINE USED AND AVAILABLE FOR CORRIDOR PTS - OG ANALYSIS ####
########################################################################

library(tidyverse)
library(amt)
library(sf)
#library(tmap)
#library(tmaptools)
library(adehabitatLT)
library(here)
library(conflicted)

conflicts_prefer(
  dplyr::filter(),
  dplyr::select()
)

rm(list = ls())


# ----------  LOAD AND PREP DATA  --------------- #

b <- readRDS(here("Data", "Derived-data", "DFs", "OG", "OG_090323.Rds"))

# Corridor points

u <- b %>% filter(at_bonepile == 0)  # load all bonepile points

u <- u %>%
  filter(!(id == "pb_32608.2008")) # AT SOME POINT, ADJUST BONEPILE DATES TO REFLECT BONEPILE-ONLY BEAR

# ------------  USED AND AVAILABLE PTS  ------------- #

##Make a list of tracks for individuals from main df 

uIDs <- unique(u$id)

trackList <- list()

for(i in 1:length(uIDs)){
  animal = filter(u, id == uIDs[i])
  trackList[[i]] = make_track(animal, 
                              .x = Xaa, 
                              .y = Yaa, 
                              .t = datetime, 
                              id = id, 
                              crs = 3338)
 
}

downSampled <- list()

for(i in 1:length(uIDs)){
  downSampled[[i]] = track_resample(trackList[[i]],
                                    rate = hours(2),
                                    tolerance = minutes(10))
}

stepsBurst <- list()

for(i in 1:length(uIDs)){
  stepsBurst[[i]] = steps_by_burst(downSampled[[i]])
}

random <- list()

for(i in 1:length(uIDs)){
  random[[2]] = random_steps(stepsBurst[[2]], n_control = 20)
}

steps <- tr2 %>% dplyr::select(id, steps)# %>% unnest(cols = steps) 


trk <- track %>% nest(data = c(-"id")) # create individual dataframes


trk_unnest <- unnest(trk)

tr2 <- steps(track)
tr3 <- tr2 %>% nest(data = c(-"id"))

# ----- GET USED AND AVAILABLE POINTS --------- #

tr3 <- tr2 %>% filter_min_n_burst(3)

ua <- steps %>% group_by(id) %>% random_steps(n_control = 20) # add random steps (gamma/von mises = default distributions)

df %>% filter(df,)

ua <- df %>% random_steps(n_control = 20) # add random steps (gamma/von mises = default distributions)

# Plot random v matched points

ggplot(ua, aes(x2_, y2_, color=case_))+
  geom_point()+
  facet_wrap(~id, scales="free")

saveRDS(ua, './non_bp_pts_used_avail.Rds')

# --------- USED AND AVAILABLE POINTS --------------  #

tr2 <- 

ua <- tr2 %>% steps_by_burst(n_control = 20) # add random steps (gamma/von mises = default distributions)

# Plot random v matched points

ggplot(ua, aes(x2_, y2_, color=case_))+
  geom_point()+
  facet_wrap(~id, scales="free")

saveRDS(ua, './non_bp_pts_used_avail.Rds')