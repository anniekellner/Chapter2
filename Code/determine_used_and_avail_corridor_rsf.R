############################################################################
#########   DETERMINE USED AND AVAILABLE POINTS FOR CORRIDOR RSF ###########
############################################################################

library(amt)
library(dplyr)
library(sf)
library(tmap)
library(tmaptools)

rm(list = ls())

source('./Code/MyFunctions.R')

# -------------   LOAD AND FORMAT DATA   ------------------------ #

cor <- readRDS('./Data/Derived-data/corridor_data.Rds')

cor <- filter(cor, case_ == "TRUE") 

## Make track with amt

track <- make_track(cor, x1_, y1_, crs = 3338, id = id) # makes one big track with all animals
trk <- track %>% nest(data = -"id") # creates dataframes for each individual

# Create random points using 99 MCP for each individual
  
rsf <- data.frame(case_ = logical(), x_ = double(), y_ = double(), id = character())
ids <- unique(cor$id)  

for(i in 1:length(ids)){
  bear_trk = trk$data[[i]]
  hr = hr_mcp(bear_trk, levels = 0.99) # tried 95 first, but 99 looks better (just like BP bears)
  rsf_pts = random_points(hr, n = 20*nrow(bear_trk), presence = bear_trk)
  rsf_pts$id = ids[i]
  rsf = rbind(rsf, rsf_pts)
}

## Plot random vs. matched points

ggplot(rsf, aes(x_, y_, color = case_)) +
  geom_point() +
  facet_wrap(~id, scales = "free")

## Plot MCP's

# Create MCP's

cor.sf <- cor %>%
  st_as_sf(., coords = c('x1_', 'y1_'), crs = 3338) %>%
  dplyr::select(id, geometry)

cor.sp <- as_Spatial(cor.sf)

mcps <- mcp(cor.sp, percent = 99)

mcp.sf <- st_as_sf(mcps) # because I hate sp

# Plot

tmap_mode('view')

tm_shape(mcps) + 
  tm_borders(lwd = 2) + 
  tm_fill(col = "id", alpha = 0.25, palette = "Accent")

###################################################################################################

#####   GUT CHECK   #####################################################################

# Look at a few bears to see what their points look like

# 20414.2009

test <- dplyr::filter(cor.sf, id == "pb_20414.2009")

tm_shape(test) + 
  tm_symbols()

# Is 20414.2009 a bonepile bear?
# No 

bone <- readRDS('./Data/Derived-data/bonepile_data.Rds')

bone.sf <- st_as_sf(bone)
bone_ids <- unique(bone$id)

# All data

all <- readRDS('./Data/bears_092921.Rds')

pb20845_all <- dplyr::filter(all, id == "pb_20845.2015")
pb20845_cor <- dplyr::filter(cor.sf, id == "pb_20845.2015")
pb20845_bone <- dplyr::filter(bone.sf, id == "pb_20845.2015")

tm_shape(pb20845_all) + 
  tm_symbols(col = "blue") + 
  tm_shape(pb20845_cor) +
  tm_symbols(col = "orange") +
  tm_shape(pb20845_bone) + 
  tm_symbols(col = "yellow")