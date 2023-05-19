###############################################
##  ARE SKIPPED FIXES RANDOM?  ################
###############################################

# October 1, 2021

# Use Kolmogorov-Smirnov test to see whether missed fixes occur randomly or non-randomly
# Skipped fixes were random in 8 bears; non-random in 13 bears

library(sf)
library(dplyr)
library(adehabitatLT)
library(lubridate)

rm(list = ls())

##### Read in data and create traj

pb <- readRDS('./Data/bears_092921.Rds') # reads in as sf, epsg = 3338

pb <- cbind(pb, st_coordinates(pb)) # separate coords from geometry columns into X and Y columns
pbdf <- as.data.frame(pb)
pbdf$obs_pt <- 1 # add column showing these are observed fixes
head(pbdf)

traj.pb<-as.ltraj(xy=pbdf[,c("X","Y")], date=pbdf$datetime, id=as.character(pbdf$id))

##### group bears based on median fix rates

traj2 <- ld(traj.pb)

traj2 <- traj2 %>%
  mutate(fix_rate=dt/3600)

avgs <- traj2 %>%
  group_by(id) %>%
  na.omit() %>%
  summarise(mean = mean(fix_rate), median = median(fix_rate)) 

avgs

traj2 <- left_join(traj2, avgs)
traj2$median <- round(traj2$median)

table(traj2$id, traj2$median)

# add to df

traj2 %>% 
  group_by(id) %>%
  slice_head() %>%
  dplyr::select(mean, median) %>%
  rename(mean_fix = mean) %>%
  rename(median_fix = median) %>%
  left_join(pbdf) -> pbdf2

head(pbdf2)

# Divide into groups

one <- filter(traj2, median == 1)
two <- filter(traj2, median > 1 & median < 4)
four <- filter(traj2, median == 4)
eight <- filter(traj2, median == 8)

pbdf.1 <- filter(pbdf2, median_fix == 1)
pbdf.2 <- filter(pbdf2, median_fix == 2)
pbdf.4 <- filter(pbdf2, median_fix == 4)
pbdf.8 <- filter(pbdf2, median_fix == 8)

traj.one <- as.ltraj(xy = one[,c('x','y')], date = one$date, id = one$id)
traj.two <- as.ltraj(xy = two[,c('x','y')], date = two$date, id = two$id)
traj.four <- as.ltraj(xy = four[,c('x','y')], date = four$date, id = four$id)
traj.eight <- as.ltraj(xy = eight[,c('x','y')], date = eight$date, id = eight$id)

# SetNAs

refda <- parse_date_time(paste(min(pb$datetime)), orders = 'ymd HMS', tz = 'US/Alaska') #set reference date 

# Nearly regular (times may still vary slightly)

NA1 <- setNA(traj.one, refda, 1, units = "hour")
NA2 <- setNA(traj.two, refda, 2, units = "hour")
NA4 <- setNA(traj.four, refda, 4, units = "hour")
NA8 <- setNA(traj.eight, refda, 8, units = "hour")

# Convert to dataframes

na1df <- ld(NA1)
na2df <- ld(NA2)
na4df <- ld(NA4)
na8df <- ld(NA8)

all1df <- na1df %>%
  dplyr::select(x:date, id) %>%
  rename(c(X = x, Y = y, datetime = date)) %>%
  full_join(pbdf.1)

all2df <- na2df %>%
  dplyr::select(x:date, id) %>%
  rename(c(X = x, Y = y, datetime = date)) %>%
  full_join(pbdf.2)

all4df <- na4df %>%
  dplyr::select(x:date, id) %>%
  rename(c(X = x, Y = y, datetime = date)) %>%
  full_join(pbdf.4)

all8df <- na8df %>%
  dplyr::select(x:date, id) %>%
  rename(c(X = x, Y = y, datetime = date)) %>%
  full_join(pbdf.8)

all1df$obs_pt[is.na(all1df$obs_pt)] <- 0
all2df$obs_pt[is.na(all2df$obs_pt)] <- 0
all4df$obs_pt[is.na(all4df$obs_pt)] <- 0
all8df$obs_pt[is.na(all8df$obs_pt)] <- 0

# Kolmogorov-Smirnov test

# Write loop that checks for uniformity of missed fixes over time for each individual bear

#In order to associate ks results with ids

ids1 <- unique(pbdf.1$id)
ids2 <- unique(pbdf.2$id)
ids4 <- unique(pbdf.4$id)
ids8 <- unique(pbdf.8$id)

all_ids <- cbind(c(ids1, ids2, ids4, ids8))

results1 <- list()

for(i in 1:length(ids)){
  pbx = filter(all1df, id == ids[i])
  pbx = arrange(pbx, datetime)
  pbx = mutate(pbx, obs_number = row_number())
  start <- first(pbx$obs_number)
  end <- last(pbx$obs_number)
  zeros <- filter(pbx, obs_pt == 0)
  zero_vec <- zeros$obs_number
  results1[[i]] <- ks.test(zero_vec, "punif", start, end)
}

r1.df <- do.call(rbind.data.frame, results1)
r1.df$fix_rate <- 1

r2.df <- do.call(rbind.data.frame, results2)
r2.df$fix_rate <- 2

r4.df <- do.call(rbind.data.frame, results4)
r4.df$fix_rate <- 4

r8.df <- do.call(rbind.data.frame, results8)
r8.df$fix_rate <- 8

ks_results <- rbind(r1.df, r2.df, r4.df, r8.df) # Final dataframe with K-S results

saveRDS(ks_results, './Results/ks_results.Rds')

# add ids to ks results df

ks <- readRDS('./Results/ks_results.Rds')
ks$ids <- all_ids

saveRDS(ks, './Results/ks_results.Rds')



