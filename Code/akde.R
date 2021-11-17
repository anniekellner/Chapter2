#########################################################
##    WHAT TYPE OF HOME RANGE ESTIMATOR TO USE  #########
#########################################################

# See whether auto-correlated kernel density estimator would work 
# 

library(ctmm)
library(dplyr)
library(stringr)
library(tidyr)
library(sf)

rm(list = ls())

source('./Code/MyFunctions.R')

pb <- readRDS('./Data/bears_092921.Rds')
pb <- st_transform(pb, 4326)

coords <- st_coordinates(pb)

pb <- st_drop_geometry(pb)

pb2 <- cbind(pb, coords)

pb2 <- pb2 %>%
  select(id, datetime, X, Y)

colnames(pb2) <- c("id", "timestamp", "longitude", "latitude") # Movebank format

pbt <- as.telemetry(pb2)

# Plots

for(i in 1:length(pbt)){ # plots
  pbx = pbt[[i]]
  m.ouf = ctmm.guess(pbx,interactive=FALSE)
  M.OUF = ctmm.fit(pbx, m.ouf) # model accouting for autocorrelation
  UD2w = akde(pbx, M.OUF, weights=TRUE) # with optimal weighting of data
  EXT = extent(UD2w, level = 0.95)
  animal = pbx@info$identity
  animal2 = gsub('\\.', '_', animal)
  png(filename = paste0('./Plots/', animal2, '.png'))
  plot(pbx,UD=UD2w,level.UD = 0.95, xlim=EXT$x,ylim=EXT$y)
  title(animal)
  dev.off()
}

akde_df <- as.data.frame(matrix(data = NA, nrow = 4))

add_column(akde_df, .before = estimate)
# Area calculation


akde <- list()

for(i in 1:length(pbt)){ # plots
  pbx = pbt[[i]]
  m.ouf = ctmm.guess(pbx,interactive=FALSE)
  M.OUF = ctmm.fit(pbx, m.ouf) # model accouting for autocorrelation
  UD2w = akde(pbx, M.OUF, weights=TRUE) # with optimal weighting of data
  akde[[i]] <- summary(UD2w)
}



