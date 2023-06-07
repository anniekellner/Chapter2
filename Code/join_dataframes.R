###########################################
###     JOIN ALL DF TO CH2 DF   ###########
###########################################

rm(list = ls())


b <- readRDS('./Data/Derived-data/DFs/bears_ch2_052823.Rds')
all <- readRDS("Data/raw_csv.Rds")

all <- all %>%
  dplyr::select(-c(data_origin, ptt:n_sats)) %>%
  mutate(id = paste(animal, year, sep = '.')) %>%
  mutate(hms = paste(hour, minute, second, sep = ':'))

all <- all %>%
  mutate(datetime = paste(date, hms))  

all$datetime <- mdy_hms(all$datetime, tz = "US/Alaska")  


t <- all %>%
  filter(id == "pb_06810.2008") %>%
  filter(datetime > "2008-09-03 00:00:00")

t <- dplyr::filter(t, datetime == as.POSIXct("2008-09-03 00:00:43"))

#saveRDS(all, './Data/raw_csv.Rds')

pb <- b %>%
  left_join(all, by = c('id', 'datetime')) %>%
  mutate(gps_lat = coalesce(gps_lat)) %>%
  mutate(gps_lon = coalesce(gps_lon)) %>%
  mutate(date = coalesce(date))