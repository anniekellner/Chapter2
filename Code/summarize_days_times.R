####################################################
##    Summarize Days Tracked       ######################
#####################################################

library(dplyr)
library(tidyr)
library(lubridate)
library(adehabitatLT)
library(sf)

rm(list = ls())

# ------------- LOAD AND PREP DATA  ------------------ #

bears <- readRDS('./Data/Derived-data/DFs/bears_ch2_092122.Rds')
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

## Days tracked

summary$DaysTrack <- as.numeric(summary$DaysTrack)

summary(summary$DaysTrack)
sd(summary$DaysTrack)



s <- summary %>%
  separate(date.begin, into = c("date.begin", "time.begin"), sep = ' ', remove = TRUE) %>% # remove = FALSE will keep original column
  separate(date.end, into = c('date.end', 'time.end'), sep = ' ', remove = TRUE) 
  
end.swim <- all.v2 %>%
  dplyr::filter(end.swim == 1)s <- s %>%
  mutate(ordinal.begin = yday(date.begin)) %>%
  mutate(ordinal.end = yday(date.end))

mean(s$ordinal.begin)

# WHETHER OR NOT BEARS ARE AT BONEPILE DURING HARVEST - help from writing group

x <- data.frame(Location = c("Chicago", "Colorado", "Colorado", "Chicago"), 
                Date = ymd(c("2008-09-06", "2008-09-07", "2008-09-13", "2008-09-05")))

x$Year <- 2008

y <- data.frame(id = c("Sam", "Kasey", "Matt", "Wendi"), 
                Location = c("Chicago", "Colorado", "Colorado", "Chicago"),
                start = ymd_hms(c("2008-09-16 18:00:37 MDT", "2008-10-03 08:00:37 MDT", "2008-08-27 05:00:00 MDT", "2012-08-27 17:00:00 MDT")),
                end = ymd_hms(c("2008-10-11 23:00:36 MDT", "2008-10-25 16:01:06 MDT", "2008-10-16 23:00:00 MDT", "2012-10-19 08:00:00 MDT")),
                Year = c(2008, 2008, 2008, 2012))

x
y
y %>% full_join(x, by = "Location") %>% rename(bone_date = Date, bone_year = Year.y) %>%
  mutate(in_int = (bone_date >= start & bone_date <= end))


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
