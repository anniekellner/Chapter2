#####################################################
####    PREPARE DATA FOR STEP SELECTION FUNCTION  ###
#####################################################

# Find step length and turning angle distributions 
# Create dataframe of used and available points

library(amt)

rm(list = ls())

# ------------ Load data ---------------------------------------- #

bears <- readRDS('./Data/all_non_bonepile_pts.Rds')

# ------------ Track  ------------------------------------------- #

track <- make_track(bears, X, Y, datetime, id = id, crs = sp::CRS("+init=epsg:3338")) # Make track with entire dataset
tr <- track %>% nest(data = -"id") # nest so each bear has an individual track (otherwise full dataset will be one big track)

tr2 <- tr %>%
  mutate(steps = map(data, function(x)
    x %>% track_resample(rate = hours(2), tolerance = minutes(20)) %>% steps_by_burst()))

# Plot histograms by id to see that they are similar

check <- tr2 %>% select(id, steps) %>% unnest(cols = steps) 

ggplot(data = check, aes(sl_)) + 
  geom_histogram()+ 
  facet_wrap(~id)

# Check for implausible values

max(check$sl_) # 9.4 km
max(check$dt_) # 2.05 hrs

# Save dataframe

#saveRDS(check, './Data/Derived-data/ssf_track.Rds')

