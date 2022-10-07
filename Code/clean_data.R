#######################################
##    Clean Dataset  ##################
#######################################

library(dplyr)
library(tidyr)
library(sf)
library(sp)
library(raster)
library(tmap)
library(lubridate)
library(data.table)

rm(list = ls())

source('./Code/MyFunctions.R')

# -------   DATA  ----------------------------------------------------------------- #

pb <- readRDS('./Data/Derived-data/DFs/bears_ch2_093022.Rds')

# Spatial data

us <- st_read('./Data/Spatial/State_Shapefile/States_NAD83_Albers.shp') # From NPS state shapefile
ak <- us %>%
  filter(STATE_ABBR == "AK") %>%
  st_transform(st_crs(pb))

plot(st_geometry(ak))
plot(st_geometry(pb), add = TRUE)


# -------- Use tmap to visualize dataset  ----------------------------------- #

tmap_mode("view")

tm_shape(ak) + 
  tm_polygons() + 
  tm_shape(pb) + 
  tm_symbols(col = "month", popup.vars = c("day")) + 
  tm_facets(by = "id") + 
  tmap_options(limits = c(facets.view = 40))

# ------ Eliminate data prior to landfall ----------------------------------- #

land.pts <- pb %>%
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

day7 <- land_bears %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  slice_head()


write.csv(pb, file = './Data/Derived-data/eliminate_points_before_arrival.csv')      # write to csv so can manually remove points before landfall

