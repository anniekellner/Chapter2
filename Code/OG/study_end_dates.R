###################################################################
#####   STUDY END DATES FOR ALL BEARS   ###########################
###################################################################

# End dates:
# Denning bears: the first day after having spent at least three successuve days in their den location
# Departing Bears: last land point before bears leave ice and do not use land for >= 7 days
# Other:
  # When collars stop transmitting data
  # For bears that use land through Dec 31, mean ordinal date of remaining bears

library(tidyverse)
library(here)
library(conflicted)

conflicts_prefer(
  dplyr::filter()
)

rm(list = ls())

# --------  LOAD DATA ------------- #

b <- readRDS(here("Data", "Derived-data", "DFs", "OG", "OG_add_depart_ice.Rds"))

# ----  ADD END DATES FOR BEARS THAT DEPART FOR ICE AND DENNING BEARS ------ #

ice <- b %>%
  filter(departure_to_ice == 1) %>%
  mutate(study_end = if_else(
    departure_to_ice == 1, 1, 0)
  ))

den <- b %>%
  filter(enter_den == 1) %>%
  mutate(study_end = if_else(
    enter_den == 1, 1, 0))

# ------ COLLAR DROPS --------- #

iceIDs <- unique(ice$id)
denIDs <- unique(den$id)

cd <- b %>%
  filter(!(id %in% iceIDs | id %in% denIDs)) %>%
  group_by(id) %>%
  slice_tail()


