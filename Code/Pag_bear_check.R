########################################################################
######    CHECK PAGANO BEARS    ########################################
########################################################################

library(tidyverse)
library(here)

rm(list = ls())

# --------  LOAD DATA ------------- #

#b <- readRDS(here("Data", "Derived-data", "DFs", "OG", "OG_add_depart_ice.Rds"))

b <- readRDS(here("Data", "Derived-data", "DFs", "OG", "OG_083023.Rds"))

# Filter bears not in Pagano database

pagNo <- b %>%
  filter(id == "pb_20418.2005" | # fix rate < 4 hrs
           id == "pb_21237.2011" | # data gap > 108 hrs
           id == "pb_32255.2008") # unclear - keep bear in unless told otherwise

pb32255 <- b %>% filter(id == "pb_32255.2008") %>% mutate(row = row_number())
pb32255 <- pb32255 %>%
  mutate(time_lapse = difftime(as.POSIXct(datetime), dplyr::lag(as.POSIXct(datetime)), units = "hours")) %>% glimpse()

max(pb32255$time_lapse, na.rm = TRUE) # no time lapses > 108 hrs

start <- first(pb32255$datetime)
end <- last(pb32255$datetime) # not sure why this bear was excluded. No data gap and hourly fix rate

ex <- b %>% filter(id == "pb_20418.2005" | 
                     id == "pb_21237.2011")

b2 <- b %>%
  anti_join(ex)

# Check other bears

pb_20529 <- b %>% filter(id == "pb_20529.2004") %>% mutate(row = row_number())
pb_20529 <- pb_20529 %>%
  mutate(time_lapse = difftime(as.POSIXct(datetime), dplyr::lag(as.POSIXct(datetime)), units = "hours")) %>% glimpse()

max(pb_20529$time_lapse, na.rm = TRUE) # no time lapses > 108 hrs

# Bear 20529.2004 has multiple data gaps > 108 hrs, including during bonepile time frame. Eliminate from analysis.

b <- b %>% filter(!id == "pb_20529.2004")
saveRDS(b, here("Data", "Derived-data", "DFs", "OG", "OG_083123.Rds"))

#saveRDS(b2, here("Data", "Derived-data", "DFs", "OG", "OG_add_depart_ice.Rds"))