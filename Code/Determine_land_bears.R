################################################
##    Determine Land Bears  ####################
################################################

library(dplyr)
library(tidyr)
library(sf)
library(sp)
library(raster)
library(lubridate)
library(data.table)

rm(list = ls())

source('./Code/MyFunctions.R')

# -------   DATA  ----------------------------------------------------------------- #

pb <- read.csv(file = "./Data/usgs_pbear_gps_ccde16_v20170131.csv")

pb <- pb %>%
  dplyr::select(animal:date) %>% # remove unnecessary columns
  filter(month > 6 & month < 11)

pbsf <- DFtoSF(pb, 3338)
#pb.spdf <- as_Spatial(pbsf)

dem_poly <- st_read('C:/Users/akell/Documents/ArcGIS/North_Slope_DEM/dem_clip_poly.shp')

land <- st_buffer(dem_poly, dist = 5000)

land2 <- st

saveRDS(land, './Data/Derived-data/dem_with_5kbuffer.shp')

st_intersects(pbsf[1,], land)

test <- st_cast(land, "MULTIPOLYGON")

test <- st_union(test)

i = 1
for(i in 1:nrow(pbsf)){
  pbsf$land[i] = ifelse(st_intersects(land, pbsf[i,]) == TRUE, 1, 0)
}

#dem[dem == 27] <- NA # turn water to NA

#dem_5kbuff <- buffer(dem, 5000, dissolve = TRUE)

#for(i in 1:nrow(pb.spdf)){
  #pb.spdf$temp[i] = extract(dem, pb.spdf[i,], na.rm = TRUE)
  #pb.spdf$land[i] = ifelse(pb.spdf$temp[i] == 27, 0, 1)
#}



# Why so many NA's?
# Because have not filtered out 'ice bears', so points are extending beyond dem


### ---------- Apply 7-day criteria ----------------------------------------------------------- ###

pbsf2 <- st_as_sf(pb.spdf) 

pbsf2$id = paste(pbsf2$animal, pbsf2$year, sep = '.')
pbsf2$ymd <- mdy(pbsf2$date)
pbsf2$datetime <- ymd_hms(paste(pbsf2$year, 
                                pbsf2$month, 
                                pbsf2$day, 
                                pbsf2$hour, 
                                pbsf2$minute,
                                pbsf2$second, sep = '-'), tz = "US/Alaska")

# count how many land points each day w/ reset by id and day
land.pts <- pbsf2 %>%
  group_by(id, ymd) %>%
  arrange(id, datetime) %>%
  drop_na(land) %>%
  mutate(all.land = cumsum(land))

flag <- land.pts %>%
  group_by(id, ymd) %>%
  arrange(id, datetime) %>%
  slice(n()) %>%
  mutate(flag = if_else(all.land == 0,0,1))

comb <- left_join(land.pts, flag)
comb[is.na(comb)] <- 1

x = comb %>% 
  group_by(id) %>% 
  arrange(id, datetime) %>% 
  mutate(time.land=ifelse(land==0 | is.na(lag(land)) | lag(land)==0 | flag==0, 
                          0,
                          difftime(datetime, lag(datetime), units="hours"))) 

x.df <- as.data.frame(x) #convert to df

setDT(x.df)
x.df[, cum.land := flag*cumsum(time.land), .(id, rleid(flag))]

x.df$cum.land = x.df$cum.land/24

land_bears <- filter(x.df, cum.land > 7)

land_ids <- unique(land_bears$id)

pbsf3 <- filter(pbsf2, id %in% land_ids)
pbsf3 <- dplyr::select(pbsf3, -temp)

saveRDS(pbsf3, file = './Data/bears_091521.Rds')
