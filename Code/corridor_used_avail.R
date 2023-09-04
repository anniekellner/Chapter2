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

# Fix rates

fix <- readRDS(here("Data", "Derived-data", "DFs", "OG", "Fix_Rates.Rds"))
#fix$median <- round(fix$median, digits = 0)

#saveRDS(fix, here("Data", "Derived-data", "DFs", "OG", "Fix_Rates.Rds"))
# ------------  USED AND AVAILABLE PTS  ------------- #

# Divide corridor animals into fix rates (downsample 1 to 2 and then run 4)

uIDs <- unique(u$id)

uFixes <- filter(fix, id %in% uIDs)

# Identify bears in each group

fixHRs1_2 <- filter(uFixes, median == 1 | median == 2)
fixHr4 <- filter(uFixes, median == 4)

TwoHrsIDs <- fixHRs1_2$id
FourHrsIDs <- fixHr4$id

# Filter used df

grp1 <- filter(u, id %in% TwoHrsIDs)
grp2 <- filter(u, id %in% FourHrsIDs)

# Make tracks

trk1 <- make_track(grp1, Xaa, Yaa, datetime, id = id, crs = 3338)
tr1 <- trk1 %>% nest(data = -"id") 

tr1_resamp <- tr1 %>% # downsample to 2-hr fix rate
  mutate(steps = map(data, function(x)
    x %>% track_resample(rate = hours(2), tolerance = minutes(20)) %>% steps_by_burst()))

steps1 <- tr1_resamp %>% dplyr::select(id, steps) %>% unnest(cols = steps) 

ua1 <- steps1 %>% group_by(id) %>% random_steps(n_control = 20) 

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