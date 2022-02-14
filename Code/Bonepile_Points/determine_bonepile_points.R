##############################################################################################
###   USE NSD TO CREATE HOME RANGES FOR BEARS BEFORE/AFTER OR AT BONEPILE   ##########
##############################################################################################

# Use this script in combination with bonepile_denning_info.xlsx 
# and use_nsd_to_determine_bonepile_arrival.R

# First look at NSD (script above) and see whether there is a clear asymptote (likely bonepile)
# Then check with script above and use tmap to plot bear locations and bp locations on map to verify


library(adehabitatLT)
library(adehabitatHR)
library(sf)
library(dplyr)
library(sp)
library(ggplot2)

rm(list = ls())

# ----------------------- Load Data  ----------------------------------------- #

source('./Code/MyFunctions.R') # for st_drop_geometry

pb <- readRDS('./Data/bears_092921.Rds') # reads in as sf object

#pb <- cbind(pb, st_coordinates(pb)) # separate coords from geometry columns into X and Y columns

#pb <- st_drop_geometry(pb)
#pbdf <- as.data.frame(pb)

# --------------------  MCP's for bears who only have info at BP    ---------- #

# Bears with all points at bonepile (reference: bonepile_denning_info.xlsx)

bp_only <- pb %>%
  filter(id == "pb_20525.2013" | id == "pb_20525.2014" | id == "pb_20586.2008" | id == "pb_32366.2014") %>%
  dplyr::select(id, geometry)


# Plot

plot(bp.sp, col = as.factor(bp.sp@data$id), pch = 16)
plot(bp_only.mcp, col = alpha(1:4, 0.5), add = TRUE)



# ------------------ Bonepile bears at the bonepile ------------------------ #

pb06810 <- pb %>%
  dplyr::filter(id == "pb_06810.2008") %>%
  dplyr::filter(datetime > "2008-09-16 18:00:37")

pb20966 <- pb %>%
  filter(id == "pb_20966.2008") %>%
  filter(datetime > "2008-08-26 16:00:00")

pb20492 <- pb %>%
  filter(id == "pb_20492.2008") %>%
  filter(datetime < "2008-10-16")

pb20520 <- pb %>% 
  filter(id == "pb_20520.2012") %>%
  filter(datetime > "2012-08-27 20:00:00" & datetime < "2012-10-19 08:00:00")

pb20735 <- pb %>%
  filter(id == "pb_20735.2009") %>%
  filter(datetime > "2009-08-09 22:00:00" & datetime < "2009-08-29 21:00:00" | datetime > "2009-09-16 01:00:00")

pb20845 <- pb %>%
  filter(id == "pb_20845.2015") %>%
  filter(datetime > "2015-09-23 20:01:29" & datetime < "2015-10-02 15:00:09")

pb21015 <- pb %>%
  filter(id == "pb_21015.2013") %>%
  filter(datetime > "2013-08-20 08:00:31" & datetime < "2013-09-27 04:00")

pb21368 <- pb %>%
  filter(id == "pb_21368.2014") %>%
  filter(datetime > "2014-08-26 04:00:00") 

pb32282 <- pb %>%
  filter(id == "pb_32282.2008") %>%
  filter(datetime > "2008-08-31 16:00:00")

pb32366_2011 <- pb %>%
  filter(id == "pb_32366.2011") %>%
  filter(datetime > "2011-08-30 12:00:00")

pb32608 <- pb %>%
  filter(id == "pb_32608.2008") %>%
  filter(datetime > "2008-08-30 10:00:00" & datetime < "2008-10-15 07:00:00") 

pb20333 <- pb %>%
  filter(id == "pb_20333.2008") %>%
  filter(datetime < "2008-09-29 19:00:00")

pb20982 <- pb %>%
  filter(id == "pb_20982.2008") %>%
  filter(datetime > "2008-09-20 16:00:00" & datetime < "2008-10-10 10:00:00")

all.bp <- bind_rows(bp_only, 
                    pb06810, 
                    pb20492, 
                    pb20520, 
                    pb20735, 
                    pb20845, 
                    pb20966, 
                    pb21015, 
                    pb21368, 
                    pb32282, 
                    pb32366_2011,
                    pb32608,
                    pb20333,
                    pb20982)

#saveRDS(all.bp, file = "./Data/all_bonepile_points.Rds")

