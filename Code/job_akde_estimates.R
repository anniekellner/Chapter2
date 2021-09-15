
library(ctmm)
library(dplyr)
library(stringr)

pb <- readRDS('./Data/bears_072321.Rds')

pb <- pb %>%
  select(id, datetime, gps_lon, gps_lat, X, Y)

colnames(pb) <- c("id", "timestamp", "longitude", "latitude", "x", "y") # Movebank format

pbt <- as.telemetry(pb)

akde <- list()

for(i in 1:length(pbt)){ # plots
  pbx = pbt[[i]]
  m.ouf = ctmm.guess(pbx,interactive=FALSE)
  M.OUF = ctmm.fit(pbx, m.ouf) # model accouting for autocorrelation
  UD2w = akde(pbx, M.OUF, weights=TRUE) # with optimal weighting of data
  akde[[i]] <- summary(UD2w)
}

saveRDS(akde, file = './Data/Derived-data/akde_list.Rds')
