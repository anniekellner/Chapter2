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

# Make tracks for two-hour group

trackList1 <- list()

for(i in 1:length(TwoHrsIDs)){
  animal = filter(grp1, id == TwoHrsIDs[i])
  trackList1[[i]] = make_track(animal, 
                              .x = Xaa, 
                              .y = Yaa, 
                              .t = datetime, 
                              id = id, 
                              crs = 3338)
  
}
    
downSampled1 <- list()

for(i in 1:length(TwoHrsIDs)){
  downSampled1[[i]] = track_resample(trackList1[[i]],
                                    rate = hours(2),
                                    tolerance = minutes(10))
}    
    
stepsBurst1 <- list()

for(i in 1:length(TwoHrsIDs)){
  stepsBurst1[[i]] = steps_by_burst(downSampled1[[i]])
}    

random1<- list()

for(i in 1:length(TwoHrsIDs)){
  random1[[i]] = random_steps(stepsBurst1[[i]], n_control = 20)
}

# Make tracks for 4 hr group

trackList2 <- list()

for(i in 1:length(FourHrsIDs)){
  animal = filter(grp2, id == FourHrsIDs[i])
  trackList2[[i]] = make_track(animal, 
                               .x = Xaa, 
                               .y = Yaa, 
                               .t = datetime, 
                               id = id, 
                               crs = 3338)
  
}


resampled2 <- list()

for(i in 1:length(FourHrsIDs)){
  resampled2[[i]] = track_resample(trackList2[[i]],
                                   rate = hours(4),
                                   tolerance = minutes(10))
}    

stepsBurst2 <- list()

for(i in 1:length(FourHrsIDs)){
  stepsBurst2[[i]] = steps_by_burst(resampled2[[i]])
}


random2<- list()

for(i in 1:length(FourHrsIDs)){
  random2[[i]] = random_steps(stepsBurst2[[i]], n_control = 20)
}

# Combine lists into dataframes





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