################################################################################
#########   CALCULATE TIME SPENT ALONG COASTAL CORRIDOR   ######################
################################################################################

library(dplyr)
library(tidyr)
library(sf)
library(lubridate)
library(adehabitatLT)

rm(list = ls())

# ----------------- LOAD AND PREP DATA   ---------------------- #

pb <- readRDS('./Data/Derived-data/DFs/bears_ch2_092122.Rds')

tz <- 'US/Alaska'

corrpts <- filter(pb, at_bonepile == 0)

# Remove corridor-only bears (because all time will count even if there are lapses)

corr2 <- corrpts %>%
  filter(!(id == "pb_20414.2009" | id == "pb_20418.2005" | id == "pb_32255.2008" | id == "pb_21237.2011"))

# ------  CALCULATE TIME SPENT  ----------------------------- #

# Look for large lapses in time

diff <- corr2 %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(timediff = difftime(datetime, lag(datetime), units = 'days')) %>%
  glimpse()

# Remove large time lapses from totals

time <- diff %>%
  filter(timediff < 2) %>% # Removes NA rows and rows with timediff > 2 days
  group_by(id) %>%
  arrange(id, datetime) %>%
  summarize(time = sum(timediff))

# Add back corridor-only bears

cor_only <- corrpts %>%
  filter(id == "pb_20414.2009" | id == "pb_20418.2005" | id == "pb_32255.2008" | id == "pb_21237.2011")

cor_only <- cor_only %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(timediff = difftime(datetime, lag(datetime), units = 'days')) %>%
  drop_na(timediff) %>%
  glimpse()
  
cor_only_sum <- cor_only %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  summarize(time = sum(timediff))
  
cor_time <- bind_rows(time, cor_only_sum)

#saveRDS(cor_time, './Data/Derived-data/DFs/Space_Use_Summaries/time_along_corr.Rds')






