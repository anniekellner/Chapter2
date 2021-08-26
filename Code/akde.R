#########################################################
##    WHAT TYPE OF HOME RANGE ESTIMATOR TO USE  #########
#########################################################

# See whether auto-correlated kernel density estimator would work 
# 

library(ctmm)
library(dplyr)
library(stringr)

rm(list = ls())

pb <- readRDS('./Data/bears_072321.Rds')

pb <- pb %>%
  select(id, datetime, gps_lon, gps_lat, X, Y)

colnames(pb) <- c("id", "timestamp", "longitude", "latitude", "x", "y") # Movebank format

pbt <- as.telemetry(pb)

for(i in 1:length(pbt)){
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




# Calculate akde object 

UD0 <- akde(pbx.t,M.IID)
UD2 <- akde(pbx.t,M.OUF)


# calculate one extent for all UDs
EXT <- extent(list(UD0,UD2,UD2w),level=0.95)

# Finally we calculate UDs with and with out accounting for autocorrelation (M.OUF versus M.IID) 
# with and without optimal weighting of the data (weights=TRUE). 
# Plot

plot(pbx.t,UD=UD0,xlim=EXT$x,ylim=EXT$y)
title(expression("IID KDE"["C"]))
plot(pbx.t,UD=UD2,xlim=EXT$x,ylim=EXT$y)
title(expression("OUF AKDE"["C"]))
plot(pbx.t,UD=UD2w,xlim=EXT$x,ylim=EXT$y)
title(expression("weighted OUF AKDE"["C"]))

# Compare estimates

summary(UD0)
summary(UD2w)

# ----  Look at 50th AKDE  ----------- #

plot(pbx.t,UD=UD0, level.UD = 0.50, xlim=EXT$x,ylim=EXT$y)
title(expression("IID KDE"["C"]))
plot(pbx.t,UD=UD2, level.UD = 0.50, xlim=EXT$x,ylim=EXT$y)
title(expression("OUF AKDE"["C"]))
plot(pbx.t,UD=UD2w,level.UD = 0.50, xlim=EXT$x,ylim=EXT$y)
title(expression("weighted OUF AKDE"["C"]))

# ----- MCP 95% ----------- #

pbx.sp <- as_Spatial(pbx.sf)

cp <- mcp(pbx.sp, percent = 95, unin = "m", unout = "km2") # MCP95

plot(cp)
plot(pbx.sp, add = TRUE)