####################################################
##    Summarize dataset       ######################
#####################################################

library(dplyr)
library(adehabitatLT)
library(sf)

rm(list = ls())

# Summarize dataset for bonepile 

bears <- readRDS('./Data/all_bonepile_points.Rds')
pb <- cbind(bears, st_coordinates(bears)) # separate coords from geometry columns into X and Y columns

pbdf <- as.data.frame(pb)

traj.pb<-as.ltraj(xy=pbdf[,c("X","Y")], date=pbdf$datetime, id=as.character(pbdf$id))
Summary.traj.pb <- summary(traj.pb)

Summary.traj.pb$DaysTrack <-round(difftime(Summary.traj.pb$date.end,Summary.traj.pb$date.begin, units="days"),digits=1)
Summary.traj.pb$Records <- Summary.traj.pb$nb.reloc-Summary.traj.pb$NAs
Summary.traj.pb$PctComplete <- round((Summary.traj.pb$nb.reloc-Summary.traj.pb$NAs)/Summary.traj.pb$nb.reloc*100,digits=1)

Summary.traj.pb %>%
  dplyr::select(id, date.begin, date.end, DaysTrack) -> summary





# Fix rates

ones <- filter(akde, median_fix == 1)
twos <- filter(akde, median_fix == 2)
fours <- filter(akde, median_fix == 4)
eight <- filter(akde, median_fix == 8)

# Missed fixes

mean(akde$missed_fixes_pct)
min(akde$missed_fixes_pct)
max(akde$missed_fixes_pct)
sd(akde$missed_fixes_pct)
