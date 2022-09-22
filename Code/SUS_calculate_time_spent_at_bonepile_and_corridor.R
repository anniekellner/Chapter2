###########################################################################
########    CALCULATE TIME SPENT AT BONEPILE AND ALONG COAST   ############
###########################################################################


library(dplyr)
library(tidyr)
library(sf)
library(adehabitatLT)
library(lubridate)

rm(list = ls())

# ----------------- LOAD AND PREP DATA   ---------------------- #

pb <- readRDS('./Data/Derived-data/DFs/bears_ch2_092122.Rds')

tz <- 'US/Alaska'

bonepts <- filter(pb, at_bonepile == 1)
corrpts <- filter(pb, at_bonepile == 0)

time_bone <- bonepts %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  dplyr::select(id, datetime) %>%
  mutate(phase = ifelse(row_number()==1, "start", "end")) %>%
  filter(!id == "pb_20735.2009")  # This bear leaves bonepile and comes back so will have to separate into two stints

# Process bear with two bonepile stays

pb20735_1 <- bonepts %>% # First time 
  filter(id == "pb_20735.2009" & datetime >= as.POSIXct("2009-08-09 22:00:00", tz = tz) & 
           datetime <= as.POSIXct("2009-08-30 08:00:00", tz = tz)) %>%
  arrange(datetime) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  dplyr::select(id, datetime) %>%
  mutate(phase = ifelse(row_number()==1, "start", "end"))

pb20735_2 <- bonepts %>% # Second time at bonepile
  filter(id == "pb_20735.2009" & datetime >= as.POSIXct("2009-09-16 01:00:00", tz = tz)) %>%
  arrange(datetime) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  dplyr::select(id, datetime) %>%
  mutate(phase = ifelse(row_number()==1, "start", "end"))

time_bone2 <- bind_rows(time_bone, pb20735_1, pb20735_2)

time_bone2[35,1] <- "pb_20735.2009.2" # so that id isn't duplicated
time_bone2[36,1] <- "pb_20735.2009.2"

piv_bone <- time_bone2 %>% 
  pivot_wider(names_from = phase, values_from = datetime) %>%
  mutate(time_spent = difftime(end, start, tz = tz, units = "days"))

# Add 20735 back together


# Merge R2n with pb df

traj.df2 <- traj.df %>%
  dplyr::select(id, date, R2n) %>%
  rename(datetime = date)

pb2 <- pb %>%
  left_join(traj.df2) 

bonepts <- pb2 %>% # Filter bonepile points
  filter(at_bonepile == 1) %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(NSDmod1 = ifelse(row_number()==1, ))


  
