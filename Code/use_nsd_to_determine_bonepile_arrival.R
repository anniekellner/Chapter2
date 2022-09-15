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

source('./Code/MyFunctions.R') # for st_drop_geometry

# Load data and create traj object

pb <- readRDS('./Data/bears_092921.Rds')

pb <- cbind(pb, st_coordinates(pb)) # separate coords from geometry columns into X and Y columns

pbdf <- as.data.frame(pb)

traj.pb<-as.ltraj(xy=pbdf[,c("X","Y")], date=pbdf$datetime, id=as.character(pbdf$id))
traj.df <- ld(traj.pb)

# Play around with r2n

uni <- unique(traj.df$id)
#test <- subset(traj.df, burst == uni[1]) # good

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

plotlist[[7]]
uni[[7]]

#-----    Plot bonepiles and look at questionable individuals ----------------------------------------------------- #

tmap_mode('view')

bone <- st_read('./Data/Spatial/Bonepiles/bonepiles.shp')

bone <- st_transform(bone, 3338)


pbx <- filter(pb, id == "pb_20525.2013")

tm_shape(bone) + 
  tm_symbols(shape = 2, col = "purple") + 
  tm_shape(pbx) + 
  tm_symbols(col = "month", palette = "YlOrRd", popup.vars = "datetime")
  

# recorded data bonepile_denning_info.xlsx

## All NSD Plots for SuppInfo

theme_set(theme_bw()) 

ggtemp = ggplot(data = temp, aes(x = date, y = R2n)) + # Example bear is 32608
  geom_point() + 
  xlab("\nCalendar Date") + 
  ylab("Net Squared Displacement from Arrival Point (m)\n") +
  theme_minimal() 

#axis.title.y = element_blank(),  # Initially thought I would not want these in the plots, but changed my mind
        #axis.text.y = element_blank()),
        #axis.ticks.y = element_blank() + 

as_tibble(temp)





