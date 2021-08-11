#########################################################
##    WHAT TYPE OF HOME RANGE ESTIMATOR TO USE  #########
#########################################################

# See whether auto-correlated kernel density estimator would work 

library(ctmm)
library(dplyr)

rm(list = ls())

pb <- readRDS('./Data/bears_072321.Rds')

pbx <- pb %>%
  filter(id == "pb_20492.2008") %>% # select bear with spotty data (this one has 35% missing fixes)
  dplyr::select(datetime, gps_lon, gps_lat, X, Y)

colnames(pbx) <- c("timestamp", "longitude", "latitude", "x", "y") # Movebank format

# See what GPS points look like

pbx.sf <- st_as_sf(pbx, coords = c("x", "y"), crs = 3338)
plot(st_geometry(pbx.sf))

pbx.t <- as.telemetry(pbx) # must be telemetry object for ctmm

# Models

M.IID <- ctmm.fit(pbx.t) # no autocorrelation
m.ouf <- ctmm.guess(pbx.t,interactive=FALSE) # automated model guess
M.OUF <- ctmm.fit(pbx.t, m.ouf)

# Calculate akde object 

UD0 <- akde(pbx.t,M.IID)
UD2 <- akde(pbx.t,M.OUF)
UD2w <- akde(pbx.t,M.OUF,weights=TRUE)

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