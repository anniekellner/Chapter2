##############################################################################
####    Using NSD to Determine First Day Bear is at Bonepile  ################
##############################################################################

library(adehabitatLT)
library(sf)
library(ggplot2)
library(plotly)
library(dplyr)

rm(list = ls())

source('./Code/MyFunctions.R') # for st_drop_geometry

# Load data and create traj object

pb <- readRDS('./Data/bears_092921.Rds')

pb <- cbind(pb, st_coordinates(pb)) # separate coords from geometry columns into X and Y columns

pbdf <- as.data.frame(pb)

traj.pb<-as.ltraj(xy=pbdf[,c("X","Y")], date=pbdf$datetime, id=as.character(pbdf$id))
traj.df <- ld(traj.pb)

# Play around with r2n

uni <- unique(traj.df$burst)
#test <- subset(traj.df, burst == uni[1]) # good

plotlist <- list()

for(i in 1:length(uni)){
  temp <- subset(x = traj.df, burst== uni[[i]])
  plotlist[[i]] = plot_ly(data = temp, x = ~date, y = ~R2n)
  }

traj.df %>%
  group_by(burst) %>%
  group_map(~ plot_ly(data = ., x = ~time, y = ~R2n, color = burst, type = "scatter")) %>%
  subplot(nrows = 3, shareX = FALSE, shareY = FALSE)

plotlist[[21]]
uni[[21]]
