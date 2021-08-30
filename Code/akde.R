#########################################################
##    WHAT TYPE OF HOME RANGE ESTIMATOR TO USE  #########
#########################################################

# See whether auto-correlated kernel density estimator would work 
# 

library(ctmm)
library(dplyr)
library(stringr)
library(tidyr)

rm(list = ls())

pb <- readRDS('./Data/bears_072321.Rds')

pb <- pb %>%
  select(id, datetime, gps_lon, gps_lat, X, Y)

colnames(pb) <- c("id", "timestamp", "longitude", "latitude", "x", "y") # Movebank format

pbt <- as.telemetry(pb)

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



