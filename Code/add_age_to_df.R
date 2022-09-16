#############################################################
######  ADD BEAR BIOLOGICAL DATA  ###########################
#############################################################

# Add age and reproductive status to pb dataframes

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(sf)

rm(list = ls())

# ----- LOAD DATA ------------------------------- #

pb <- readRDS('./Data/bears_092921.Rds') 

repro <- readRDS('./Data/Derived-data/Repro.Rds')
age <- read.csv('./Data/kate_offspring_info.csv')

## Edit Kate spreadsheet so plays nice with mine

age <- dplyr::select(age, mom.id, cap.date, mom.age, cub.1, age.class.1, cub.2, age.class.2)

age <- age[!is.na(age$mom.id),]

## Add 'pb_' to animal id's

# Moms

age$mom.id <- as.character(age$mom.id)
age$mom.id <- paste0("pb_", age$mom.id)

age[9:11, 1] <- "pb_06810" # add 0 because was just 6810



## Format dates

age <- separate(age, cap.date, into = c("mdy", "time"), sep = "^\\S*\\K\\s+")

age$mdy <- mdy(age$mdy)
age$year <- year(age$mdy)

# Select relevant rows in Kate data

ak <- unique(pb$animal)

age <- age[age$mom.id %in% ak,] # Have age info on 16/19 bears

# Calculate birthyear and prep df for join

age2 <- age %>%
  group_by(mom.id) %>%
  slice_head() %>%
  mutate(birthyear = year - mom.age) %>%
  select(mom.id, birthyear) %>%
  rename("animal" = "mom.id")

# Join with my dataset

pb <- st_drop_geometry(pb)

pb2 <- pb %>%
  left_join(age2) %>%
  mutate(age = year - birthyear)

pb2 <- select(pb2, -c(data_origin, birthyear))

saveRDS(pb2, './Data/bears_091622.Rds')


# ----- Looked to see if any of the three bears without age data were listed in Kate's data as cubs ---------------- #

#unique(age$mom.id) 
#setdiff(ak, unique(age$mom.id)) # pb_20982, pb_21237, pb_32608

# See if any of the missing id's can be found in the cub categories

#age$cub.1 <- as.character(age$cub.1)
#age$cub.1 <- paste0("pb_", age$cub.1) 

#age$cub.2 <- as.character(age$cub.2)
#age$cub.2 <- paste0("pb_", age$cub.2) 

#age[age$cub.1 %in% ak,] # not any of the missing ones
#age[age$cub.2 %in% ak,] # not any of the missing ones
