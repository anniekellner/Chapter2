##############################################################################################
###   USE NSD TO CREATE HOME RANGES FOR BEARS BEFORE/AFTER OR AT BONEPILE   ##########
##############################################################################################

# Use this script in combination with bonepile_denning_info.xlsx 
# and use_nsd_to_determine_bonepile_arrival.R

# First look at NSD (script above) and see whether there is a clear asymptote (likely bonepile)
# Then check with script above and use tmap to plot bear locations and bp locations on map to verify

### CHECK BP DATES AGAINST TIME SPENT AT BONEPILE FOR V1 BEARS (CSV IN GOOGLE SHEETS)

library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library(here)
library(conflicted)

conflicts_prefer(
  dplyr::filter()
)

rm(list = ls())

# ----------------------- LOAD AND PREP DATA  ----------------------------------------- #

## Load

pb <- readRDS(here("Data", "Derived-data", "DFs", "OG", "OG.RDS"))

time_at_bp <- readRDS(here("Data", "Derived-data", "DFs", "Space_Use_Summaries", "time_at_bonepile.Rds"))
#write_csv(time_at_bp, here("Data", "Derived-data", "Bonepile", "time_at_bonepile.csv"))

# Spatial

bones <- st_read(here("Data", "Spatial", "Bonepiles", "bonepiles.shp"))
bones <-st_transform(bones, 3338)

## Prep

# Make pb into sf object

pb <- st_as_sf(pb, coords = c('gps_lon', 'gps_lat'), crs = 4326)
pb <- cbind(st_coordinates(pb), pb)
pb <- pb %>% # preserve lat/lon
  rename(gps_lon = X) %>%
  rename(gps_lat = Y)

pb <- st_transform(pb, crs = 3338)
pb <- cbind(st_coordinates(pb), pb)
pb <- pb %>% # preserve Alaska Albers X and Y
  rename(Xaa = X) %>%
  rename(Yaa = Y)

# erase 20333.2008 from time_at_bonepile

time_at_bp[2,] <- NA
time_at_bp <- na.omit(time_at_bp)

# ------------------ BONEPILE POINTS ------------------------ #

# DOES NOT WORK CORRECTLY UNLESS I USE as.POSIXct() AND SPECIFY TIMEZONE

## Bears added to new dataset

tz <- 'US/Alaska'

pb20529.2005 <- pb %>%
  dplyr::filter(id == "pb_20529.2005") %>%
  dplyr::filter(datetime >= as.POSIXct("2005-09-20 18:01:00", tz = tz) & 
                  datetime <= as.POSIXct("2005-10-11 2:00:00", tz = tz)) 

pb20965.2008 <- pb %>%
  dplyr::filter(id == "pb_20965.2008") %>%
  dplyr::filter(datetime >= as.POSIXct("2008-09-15 18:00:00", tz = tz) & 
                  datetime <= as.POSIXct("2008-10-09 03:00:00", tz = tz)) 

pb20975.2008 <- pb %>%
  dplyr::filter(id == "pb_20975.2008") %>%
  dplyr::filter(datetime >= as.POSIXct("2008-09-09 01:00:00", tz = tz)) # bear remains at bp until end of study

pb21264.2011 <- pb %>%
  dplyr::filter(id == "pb_21264.2011") %>%
  dplyr::filter(datetime >= as.POSIXct("2011-09-09 00:00:00", tz = tz) & 
                  datetime <= as.POSIXct("2011-10-01 16:00:00", tz = tz)) 

pb21358.2013 <- pb %>%
  dplyr::filter(id == "pb_21358.2013") %>%
  dplyr::filter(datetime >= as.POSIXct("2013-08-25 22:00:00", tz = tz) & 
                  datetime <= as.POSIXct("2013-09-25 00:00:00", tz = tz)) 

## Bears from old Dataset

pb20586.2008 <- pb %>%
  filter(id == "pb_20586.2008") %>% 
  filter(datetime <= as.POSIXct("2008-10-14 07:00:36", tz = tz))

pb20525.2013 <- pb %>%
  filter(id == "pb_20525.2013") %>%
  filter(datetime >= as.POSIXct("2013-09-12 08:01:29") & 
           datetime <= as.POSIXct("2013-10-26 06:00:40"))

pb06810.2008 <- pb %>%
  filter(id == "pb_06810.2008") %>%
  filter(datetime >= as.POSIXct("2008-09-17 02:00:37", tz = tz) & 
           datetime <= as.POSIXct("2008-10-12 07:00:36"))

pb20492.2008 <- pb %>%
  filter(id == "pb_20492.2008") %>%
  filter(datetime <= as.POSIXct("2008-10-17", tz = tz))

pb20520.2012 <- pb %>% 
  filter(id == "pb_20520.2012") %>%
  filter(datetime >= as.POSIXct("2012-08-28 01:00:00", tz = 'US/Alaska') & 
           datetime <= as.POSIXct("2012-10-19 16:00:00", tz = 'US/Alaska'))

pb20735.2009 <- pb %>%
  filter(id == "pb_20735.2009") %>%
  filter(datetime >= as.POSIXct("2009-08-10 06:00:00", tz = tz) &
           datetime < as.POSIXct("2009-08-30 05:00:00", tz = tz) |
           datetime >= as.POSIXct("2009-09-16 09:00:00", tz = tz) & 
           datetime <= as.POSIXct("2009-10-13 07:00:00", tz = tz))

pb20845.2015 <- pb %>%
  filter(id == "pb_20845.2015") %>%
  filter(datetime >= as.POSIXct("2015-09-24 04:01:29", tz = tz) & 
           datetime < as.POSIXct("2015-10-03 12:00:47", tz = tz))
  

pb20966.2008 <- pb %>%
  filter(id == "pb_20966.2008") %>%
  filter(datetime >= as.POSIXct("2008-08-26 18:00:36", tz = tz) & 
           datetime <= as.POSIXct("2008-10-15 07:00:37"))

pb20982.2008 <- pb %>%
  filter(id == "pb_20982.2008") %>%
  filter(datetime >= as.POSIXct("2008-09-22 01:00:00", tz = tz) & 
           datetime < as.POSIXct("2008-10-05 10:00:00", tz = tz))

pb21015.2013 <- pb %>%
  filter(id == "pb_21015.2013") %>%
  filter(datetime >= as.POSIXct("2013-08-20 20:00:30", tz = tz) & 
           datetime < as.POSIXct("2013-09-27 12:00:30", tz = tz))

pb21368.2014 <- pb %>%
  filter(id == "pb_21368.2014") %>%
  filter(datetime >= as.POSIXct("2014-08-26 04:00:00", tz = tz) & 
           datetime <= as.POSIXct("2014-10-26 06:00:49", tz = tz)) 

pb32282.2008 <- pb %>%
  filter(id == "pb_32282.2008") %>%
  filter(datetime >= as.POSIXct("2008-08-31 23:00:36", tz = tz) & 
           datetime <= as.POSIXct("2008-10-05 07:00:20", tz = tz))

pb32366.2011 <- pb %>%
  filter(id == "pb_32366.2011") %>%
  filter(datetime >= as.POSIXct("2011-08-30 20:00:00", tz = tz) & 
           datetime <= as.POSIXct("2011-10-22 08:00:00", tz = tz))

pb32366.2014 <- pb %>%
  filter(id == "pb_32366.2014") %>%
  filter(datetime >= as.POSIXct("2014-09-02 22:00:29", tz = tz) & 
           datetime <= as.POSIXct("2014-10-26 06:00:30", tz = tz))

pb32608.2008 <- pb %>%
  filter(id == "pb_32608.2008") %>%
  filter(datetime >= as.POSIXct("2008-08-30 18:00:00", tz = tz) & 
           datetime <= as.POSIXct("2008-10-15 14:00:00", tz = tz)) 

pb20525.2014 <- pb %>%
  filter(id == "pb_20525.2014") %>% 
  filter(datetime >= as.POSIXct("2014-08-29 02:00:29", tz = tz) & 
           datetime <= as.POSIXct("2014-10-26 06:00:22", tz = tz))


# Combine

all.bp <- bind_rows(pb06810.2008,
                    pb20492.2008, 
                    pb20520.2012, 
                    pb20735.2009, 
                    pb20845.2015, 
                    pb20966.2008, 
                    pb21015.2013, 
                    pb21368.2014, 
                    pb32282.2008, 
                    pb32366.2011,
                    pb32608.2008,
                    pb20982.2008,
                    pb20529.2005,
                    pb20965.2008,
                    pb20975.2008,
                    pb21264.2011,
                    pb21358.2013,
                    pb20525.2013,
                    pb20586.2008,
                    pb32366.2014,
                    pb20525.2013,
                    pb20525.2014)

# Check that everything is there - LOOKS GOOD

bpIDs <- unique(all.bp$id) # 21 bear ids 
allIDs <- unique(pb$id)

setdiff(allIDs, bpIDs) # Difference of four - looks good!

# -------- PLOT CHECKS ------------------------------------- #

# Plot - looks good 9/21/22

tmap_mode('view')

tm_shape(bones) + 
  tm_dots(col = "blue", size = 0.5) + 
  tm_shape(all.bp) + 
  tm_symbols(col = "id", popup.vars = c("id", "ymd"))

# Points that look strange

# pb_20529.2005 - not really at bonepile. Check other script and/or see if denning bear 
# pb_20520.2012 - see if bp end date might be too late
# pb_20975.2008 - check start date - might be too early
# pb20492.2008 - check end date and see if too late
# pb20485.2015 see if end is too late

# ------- Add 'at_bonepile' column to main df -------------------------------- #

all.bp$at_bonepile <- 1

pb2 <- pb %>%
  left_join(all.bp) %>%
  replace_na(list(at_bonepile = 0))

pbsf <- st_as_sf(pb2) %>%
  st_set_crs(3338) 

pbsf$at_bonepile <- as.factor(pbsf$at_bonepile)

# Plot again

tm_shape(bones) + 
  tm_dots(col = "blue", size = 0.5) + 
  tm_shape(pbsf) + 
  tm_dots(col = "at_bonepile")

# ------------------- Save dataframe  ---------------------------------------- #

#saveRDS(pb2, './Data/Derived-data/DFs/bears_ch2_092122.Rds')

