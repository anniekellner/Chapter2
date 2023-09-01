##############################################################################
####    Using NSD to Determine First Day Bear is at Bonepile  ################
##############################################################################

# recorded data bonepile_denning_info.xlsx - round 1
# recorded data OG-Bonepile-denning-info.csv - OG Analysis (saved to Data/Derived-data/Bonepile in repo)
  # This file is also in Google Sheets in Polar Bears/OG/Bonepile (or something like that)

## TO DO: 
  # Re-do 20525.2014, 32366.2014
  # Add bonepile info to spreadsheet 
  # Write Methods into document


library(sf)
library(tmap)
library(tmaptools)
library(adehabitatLT)
library(ggplot2)
library(plotly)
library(tidyverse)
library(here)
library(conflicted)

conflicts_prefer(
  dplyr::filter()
)

rm(list = ls())

# ------- LOAD AND PREP DATA  ----------- #

## Bears

pb <- readRDS(here("Data", "Derived-data", "DFs", "OG", "OG.Rds"))

pbsf <- st_as_sf(pb, coords = c("gps_lon", "gps_lat"), crs = 4326) 
pbsf <- st_transform(pbsf, crs = 3338) # Albers Alaska

pba <- cbind(pbsf, st_coordinates(pbsf)) # separate coords from geometry columns into X and Y columns

pbdf <- st_drop_geometry(pba)

years <- unique(pbdf$year)

## Bonepile

# Shapefile 

bone <- st_read(here("Data", "Spatial", "Bonepiles", "bonepiles.shp"))
bone <- st_transform(bone, crs = 3338)

# Spreadsheet with dates by year

boneDates <- read_csv(here("Data", "Bonepile_Dates.csv"))

boneDates$Dates <- mdy(boneDates$Dates)
boneDates$Year <- year(boneDates$Dates)

# Bonepile bears from previous work

boneBears <- readRDS(here("Data", "Derived-data", "DFs", "Space_Use_Summaries", "time_at_bonepile.Rds"))
boneBears$Bonepile_Bear <- 1

boneIDs <- unique(boneBears$id)
corIDs <- c("pb_20414.2009", "pb_20418.2005", "pb_32255.2008", "pb_21237.2011")

oldbears <- c(boneIDs, corIDs)

NSDbears <- pbdf %>%
  filter(!id %in% oldbears) 
  
unique(NSDbears$id)


# ----  CREATE TRAJ OBJECT  ------ #

traj.pb<-as.ltraj(xy=NSDbears[,c("X","Y")], date=NSDbears$datetime, id=as.character(NSDbears$id))
traj.df <- ld(traj.pb)

# Play around with r2n

ids <- unique(traj.df$id)

# Create individual interactive plots so can assess dates for bp arrival and departure

plotlist <- list()

for(i in 1:length(ids)){
  temp = subset(x = traj.df, id== ids[[i]])
  plotlist[[i]] = plot_ly(data = temp, x = ~date, y = ~R2n, type = "scatter") # https://plotly.com/r/reference/#scatter
  }

plotlist[[1]]
ids[[1]]

# Using plotly, can view sheet in browser and zoom in on time in question to get precise dates

#-----    Plot bonepiles and look at questionable individuals ----------------------------------------------------- #

# Questionable: Need to determine bonepile radius in order to determine bonepile pts
  # "pb_20446.2009" - only visits bonepile briefly 
  # "pb_20529.2004" - same as above, can cross-reference with bonepile dates

## PUT BONEPILE SPREADSHEET INTO GOOGLE SHEETS SO CAN VIEW ON SAME SCREEN 

tmap_mode('view')

# Bear points for whole study

pbx <- filter(pbsf, id == "pb_21358.2013")

tm_shape(bone) + 
  tm_symbols(col = "purple") + 
  tm_shape(pbx) + 
  tm_symbols(col = "month", palette = "YlOrRd", popup.vars = "ymd")

# Bonepile dates

filter(boneDates, Year == 2013 & Bonepile == "Cross")



## IF NECESSARY ##

pbxBP <- filter(pbx, month == 10)
unique(pbxBP$ymd)

tmap_options(max.categories = 35)

tm_shape(bone) + 
  tm_symbols(col = "purple") + 
  tm_shape(pbxBP) + 
  tm_symbols(col = "ymd", palette = "magma", popup.vars = "ymd")


  









