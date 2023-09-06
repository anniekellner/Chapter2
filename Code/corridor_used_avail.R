########################################################################
#####   DETERMINE USED AND AVAILABLE FOR CORRIDOR PTS - OG ANALYSIS ####
########################################################################

library(tidyverse)
library(amt)
library(sf)
library(tmap)
library(tmaptools)
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

## NEED TO PUT ID BACK INTO LISTS
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
  names(trackList1)[i] = TwoHrsIDs[i]
}
    
downSampled1 <- list()

for(i in 1:length(TwoHrsIDs)){
  downSampled1[[i]] = track_resample(trackList1[[i]],
                                    rate = hours(2),
                                    tolerance = minutes(10))
  names(downSampled1)[i] = TwoHrsIDs[i]
}    
    
stepsBurst1 <- list()

for(i in 1:length(TwoHrsIDs)){
  stepsBurst1[[i]] = steps_by_burst(downSampled1[[i]])
  names(stepsBurst1)[i] = TwoHrsIDs[i]
}    

random1<- list()

for(i in 1:length(TwoHrsIDs)){
  random1[[i]] = random_steps(stepsBurst1[[i]], n_control = 20)
  names(random1)[i] = TwoHrsIDs[i]
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
  names(trackList2)[i] = FourHrsIDs[i]
}


resampled2 <- list()

for(i in 1:length(FourHrsIDs)){
  resampled2[[i]] = track_resample(trackList2[[i]],
                                   rate = hours(4),
                                   tolerance = minutes(10))
  names(resampled2)[i] = FourHrsIDs[i]
}    

stepsBurst2 <- list()

for(i in 1:length(FourHrsIDs)){
  stepsBurst2[[i]] = steps_by_burst(resampled2[[i]])
  names(stepsBurst2)[i] = FourHrsIDs[i]
}


random2<- list()

for(i in 1:length(FourHrsIDs)){
  random2[[i]] = random_steps(stepsBurst2[[i]], n_control = 20)
  names(random2)[i] = FourHrsIDs[i]
}

# Combine lists into dataframes

ssf_2hr_ua <- map_df(random1, ~as.data.frame(.x), .id = "id")
ssf_4hr_ua <- map_df(random2, ~as.data.frame(.x), .id = "id")

ssf2_naomit <- na.omit(ssf_2hr_ua) # looks like NA's occurred when fix rate was > hour specified. Remove those points and note in methods.
ssf4_naomit <- na.omit(ssf_4hr_ua)

burst1 <- map_df(stepsBurst1, ~as.data.frame(.x), .id = "id")
burst2 <- map_df(stepsBurst2, ~as.data.frame(.x), .id = "id")

# ---- Get mean overall step length per hour for corridor bears  --------- #


#saveRDS(ssf2_naomit, here("Data", "Derived-data", "DFs", "OG", "ssf_2h_ua.Rds"))
#saveRDS(ssf4_naomit, here("Data", "Derived-data", "DFs", "OG", "ssf_4h_ua.Rds"))

# NA stats:
# 3.9% of points for 2 hr group: 541.7/13861 
# 3.0% for 4 hr group: 22.85/767

# ------- PLOT CHECK  ----------- #

# Use 4 hr points because there are fewer, but still get an idea of used v. available

sSF4 <- st_as_sf(ssf4_naomit, coords = c('x2_', 'y2_'), crs = 3338)
true <- filter(sSF4, case_ == TRUE)
false <- filter(sSF4, case_ == FALSE)

tmap_mode('view')

tm_shape(false) + 
  tm_symbols(col = "grey") + 
  tm_shape(true) + 
  tm_symbols(col = "red")

