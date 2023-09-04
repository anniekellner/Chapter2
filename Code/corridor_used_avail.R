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
b <- as.data.frame(b)

# Corridor points

u <- b %>% filter(at_bonepile == 0)  # load all bonepile points
u <- filter(!id = "pb_32608.2008") # AT SOME POINT, ADJUST BONEPILE DATES TO REFLECT BONEPILE-ONLY BEAR

# ------  SUMMARY STATS ----------------- #

# Create track in adehabitatLT

ltraj <- as.ltraj(xy=u[,c("Xaa","Yaa")], date=u$datetime, id=as.character(u$id))

summary <- summary(ltraj)

summary$DaysTrack <-round(difftime(summary$date.end, summary$date.begin, units="days"),digits=1)
summary <- dplyr::select(summary, id, date.begin, date.end, DaysTrack)
summary

summary$DaysTrack <- as.numeric(summary$DaysTrack)

summary(summary)

sd(summary$DaysTrack)
```
