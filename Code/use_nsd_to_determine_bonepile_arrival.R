##############################################################################
####    Using NSD to Determine First Day Bear is at Bonepile  ################
##############################################################################

library(adehabitatLT)
library(sf)
library(ggplot2)
library(plotly)
library(dplyr)
library(tmap)
library(tmaptools)

rm(list = ls())

# Load data and create traj object

pb <- readRDS('./Data/Derived-data/DFs/bears_ch2_092122.Rds')
pbsf <- st_as_sf(pb) %>% st_set_crs(3338)

pb <- cbind(pbsf, st_coordinates(pbsf)) # separate coords from geometry columns into X and Y columns

pbdf <- as.data.frame(pb)

traj.pb<-as.ltraj(xy=pbdf[,c("X","Y")], date=pbdf$datetime, id=as.character(pbdf$id))
traj.df <- ld(traj.pb)

# Play around with r2n

uni <- unique(traj.df$id)

# Create individual interactive plots so can assess dates for bp arrival and departure

plotlist <- list()

for(i in 1:length(uni)){
  temp = subset(x = traj.df, id== uni[[i]])
  plotlist[[i]] = plot_ly(data = temp, x = ~date, y = ~R2n, type = "scatter") # https://plotly.com/r/reference/#scatter
  }

#traj.df %>%
  #group_by(burst) %>%
  #group_map(~ plot_ly(data = ., x = ~time, y = ~R2n, color = burst, type = "scatter")) %>%
  #subplot(nrows = 3, shareX = FALSE, shareY = FALSE)

plotlist[[5]]
uni[[5]]

#-----    Plot bonepiles and look at questionable individuals ----------------------------------------------------- #

tmap_mode('view')

bone <- st_read('./Data/Spatial/Bonepiles/bonepiles.shp')

bone <- st_transform(bone, 3338)


pbx <- filter(pb, id == "pb_32255.2008")

tm_shape(bone) + 
  tm_symbols(shape = 2, col = "purple") + 
  tm_shape(pbx) + 
  tm_symbols(col = "month", palette = "YlOrRd", popup.vars = "datetime")
  

# recorded data bonepile_denning_info.xlsx







