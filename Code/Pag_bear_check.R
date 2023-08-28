########################################################################
######    CHECK PAGANO BEARS    ########################################
########################################################################

library(tidyverse)

# --------  LOAD DATA ------------- #

b <- readRDS(here("Data", "Derived-data", "DFs", "OG", "OG_add_depart_ice.Rds"))

# Filter bears not in Pagano database

pagNo <- b %>%
  filter(id == "pb_20418.2005" | # fix rate < 4 hrs
           id == "pb_21237.2011" | # data gap > 108 hrs
           id == "pb_32255.2008") # unclear

pb32255 <- b %>% filter(id == "pb_32255.2008") %>% mutate(row = row_number())
pb32255 <- pb32255 %>%
  mutate(time_lapse = difftime(as.POSIXct(datetime), dplyr::lag(as.POSIXct(datetime)), units = "hours")) %>% glimpse()

max(pb32255$time_lapse, na.rm = TRUE) # no time lapses > 108 hrs

start <- first(pb32255$datetime)
end <- last(pb32255$datetime) # not sure why this bear was excluded. No data gap and hourly fix rate
