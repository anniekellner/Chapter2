##############################################################################################
###   USE NSD TO CREATE HOME RANGES FOR BEARS BEFORE/AFTER OR AT BONEPILE   ##########
##############################################################################################

# Use this script in combination with bonepile_denning_info.xlsx 
# and use_nsd_to_determine_bonepile_arrival.R

# First look at NSD (script above) and see whether there is a clear asymptote (likely bonepile)
# Then check with script above and use tmap to plot bear locations and bp locations on map to verify

library(sf)
library(dplyr)
library(tidyr)
library(tmap)
library(tmaptools)
library(lubridate)

rm(list = ls())

# ----------------------- Load Data  ----------------------------------------- #

pb <- readRDS('./Data/Derived-data/DFs/bears_ch2_092122.Rds') # reads in as df 


# --------------------  Bonepile-only Bears   ---------- #

  # pb_20586.2008
  # pb_20525.2013
  # pb_20525.2014
  # pb_32366.2014


bp_only <- pb %>%
  filter(id == "pb_20525.2013" | id == "pb_20586.2008" |
           id == "pb_32366.2014" | id == "pb_20525.2014") 


# ------------------ Bonepile points for bears that travel ------------------------ #

# DOES NOT WORK CORRECTLY UNLESS I USE as.POSIXct() AND SPECIFY TIMEZONE

tz <- 'US/Alaska'

pb06810.2008 <- pb %>%
  dplyr::filter(id == "pb_06810.2008") %>%
  dplyr::filter(datetime >= as.POSIXct("2008-09-16 18:00:37", tz = 'US/Alaska')) 

pb20333.2008 <- pb %>%
  filter(id == "pb_20333.2008") %>%
  filter(datetime >= as.POSIXct("2008-10-03 08:00:00", tz = tz))

pb20492.2008 <- pb %>%
  filter(id == "pb_20492.2008") %>%
  filter(datetime <= as.POSIXct("2008-10-17", tz = tz))

pb20520.2012 <- pb %>% 
  filter(id == "pb_20520.2012") %>%
  filter(datetime >= as.POSIXct("2012-08-27 17:00:00", tz = 'US/Alaska') & 
           datetime <= as.POSIXct("2012-10-19 08:00:00", tz = 'US/Alaska'))

pb20735.2009 <- pb %>%
  filter(id == "pb_20735.2009") %>%
  filter(datetime >= as.POSIXct("2009-08-09 22:00:00", tz = tz) &
           datetime < as.POSIXct("2009-08-30 08:00:00", tz = tz) |
           datetime >= as.POSIXct("2009-09-16 01:00:00", tz = tz))

pb20845.2015 <- pb %>%
  filter(id == "pb_20845.2015") %>%
  filter(datetime >= as.POSIXct("2015-09-23 20:01:29", tz = tz) & datetime < as.POSIXct("2015-10-03 05:00:19", tz = tz))
  

pb20966.2008 <- pb %>%
  filter(id == "pb_20966.2008") %>%
  filter(datetime >= as.POSIXct("2008-08-26 10:00:00", tz = tz))

pb20982.2008 <- pb %>%
  filter(id == "pb_20982.2008") %>%
  filter(datetime >= as.POSIXct("2008-09-21 17:00:00", tz = tz) & datetime < as.POSIXct("2008-10-05 03:00:00", tz = tz))


pb21015.2013 <- pb %>%
  filter(id == "pb_21015.2013") %>%
  filter(datetime >= as.POSIXct("2013-08-20 12:00:30", tz = tz) & datetime < as.POSIXct("2013-09-27 06:00", tz = tz))

pb21368.2014 <- pb %>%
  filter(id == "pb_21368.2014") %>%
  filter(datetime >= as.POSIXct("2014-08-26 04:00:00", tz = tz)) 

pb32282.2008 <- pb %>%
  filter(id == "pb_32282.2008") %>%
  filter(datetime >= as.POSIXct("2008-08-31 15:00:00", tz = tz))

pb32366.2011 <- pb %>%
  filter(id == "pb_32366.2011") %>%
  filter(datetime >= as.POSIXct("2011-08-30 12:00:00", tz = tz))

pb32608.2008 <- pb %>%
  filter(id == "pb_32608.2008") %>%
  filter(datetime >= as.POSIXct("2008-08-30 10:00:00", tz = tz) & datetime < as.POSIXct("2008-10-15 07:00:00", tz = tz)) 


# Combine

all.bp <- bind_rows(bp_only, 
                    pb06810.2008,
                    pb20333.2008,
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
                    pb20982.2008)

# Check that everything is there

unique(all.bp$id) # 17 bear ids - this is correct because there are 4 non-bonepile bears


# -------- PLOT CHECKS ------------------------------------- #

# Geometry disappeared from points because the st_drop_geometry() fx in {sf} does not separate the coordinates into columns first

# Add back geometry 

all.bp2 <- all.bp %>%
  st_as_sf() %>%
  st_set_crs(3338)


# Add bonepile shp

bones <- st_read('./Data/Spatial/Bonepiles/bonepiles.shp') %>%
  st_transform(3338)

# Plot - looks good 9/21/22

tmap_mode('view')

#tm_shape(bones) + 
  #tm_dots(col = "blue", size = 0.5) + 
  #tm_shape(all.bp2) + 
  #tm_dots(col = "purple")

# Points that look strange

# pb_20520.2012 - A one-day jaunt. OK. 
# pb_20982 - revised
# pb_20492 - OK

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

