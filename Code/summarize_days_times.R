####################################################
##    Summarize Days Tracked       ######################
#####################################################

# 21237.2011 was not collared on land, but bear is missing data between July and September. Bear appears on land in September.  
# 20525.2014 did not really swim, but walked off ice onto land.  


library(dplyr)
library(tidyr)
library(lubridate)
library(adehabitatLT)
library(sf)

rm(list = ls())

# ------------- LOAD AND PREP DATA  ------------------ #

bears <- readRDS('./Data/Derived-data/DFs/bears_ch2_092122.Rds')
all.v2 <- readRDS('./Data/Derived-data/DFs/all_v2.Rds') # to get landing points

pb <- st_as_sf(bears, crs = 3338)

pb <- cbind(pb, st_coordinates(pb)) # separate coords from geometry columns into X and Y columns

pbdf <- as.data.frame(pb)

# ---------- TRAJ -------------------------------------- #

traj.pb<-as.ltraj(xy=pbdf[,c("X","Y")], date=pbdf$datetime, id=as.character(pbdf$id))

Summary.traj.pb <- summary(traj.pb)

Summary.traj.pb$DaysTrack <-round(difftime(Summary.traj.pb$date.end,Summary.traj.pb$date.begin, units="days"),digits=1)
Summary.traj.pb$Records <- Summary.traj.pb$nb.reloc-Summary.traj.pb$NAs
Summary.traj.pb$PctComplete <- round((Summary.traj.pb$nb.reloc-Summary.traj.pb$NAs)/Summary.traj.pb$nb.reloc*100,digits=1)

Summary.traj.pb %>%
  dplyr::select(id, date.begin, date.end, DaysTrack) -> summary

# ----- SUMMARIZE --------------------------------- #

# Days tracked

summary$DaysTrack <- as.numeric(summary$DaysTrack)

summary(summary$DaysTrack)
sd(summary$DaysTrack)

# Deal with dates and times

# Landfall

l <- bears %>% # 'landfall' is misnamed for bears that were collared on land - change
  filter(landfall == 1) 

ch2 <- unique(l$id)  
  
end.swim <- all.v2 %>%
  dplyr::filter(end.swim == 1)

end <- unique(end.swim$id)

dplyr::setdiff(ch2, end) # look at 20525.2014 and 

b11 <- all.v2 %>%
  dplyr::filter(id == "pb_21237.2011") 

b14l <- l %>%


s <- summary %>%
  separate(date.begin, into = c("date.begin", "time.begin"), sep = ' ', remove = TRUE) %>% # remove = FALSE will keep original column
  separate(date.end, into = c('date.end', 'time.end'), sep = ' ', remove = TRUE) 
  
end.swim <- all.v2 %>%
  dplyr::filter(end.swim == 1)s <- s %>%
  mutate(ordinal.begin = yday(date.begin)) %>%
  mutate(ordinal.end = yday(date.end))

mean(s$ordinal.begin)




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
