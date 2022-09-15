#############################################################
######  ADD BEAR BIOLOGICAL DATA  ###########################
#############################################################

# Add age and reproductive status to pb dataframes

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

rm(list = ls())

# ----- LOAD DATA ------------------------------- #

pb <- readRDS('./Data/bears_092921.Rds') 

repro <- readRDS('./Data/Derived-data/Repro.Rds')
age <- read.csv('./Data/kate_offspring_info.csv')

## Edit Kate spreadsheet so plays nice with mine

age <- dplyr::select(age, mom.id, cap.date, mom.age, cub.1, age.class.1, cub.2, age.class.2)

age <- age[!is.na(age$mom.id),]

# Add 'pb_' to animal id's

age$mom.id <- as.character(age$mom.id)
age$mom.id <- paste0("pb_", age$mom.id)

age[9:11, 1] <- "pb_06810" # add 0 because was just 6810 

age <- separate(age, cap.date, into = c("mdy", "time"), sep = "^\\S*\\K\\s+")

age$mdy <- mdy(age$mdy)
age$year <- year(age$mdy)

# Select relevant rows in Kate data

ak <- unique(pb$animal)
age <- age[age$mom.id %in% ak,]
unique(age$mom.id) # Have age info on 16/19 individuals

setdiff(ak, unique(age$mom.id)) # pb_20982, pb_21237, pb_32608


