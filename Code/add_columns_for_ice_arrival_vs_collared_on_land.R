########################################################
####    ADD COLUMNS TO DISTINGUISH BEARS    #############
####  THAT ARRIVED FROM ICE VS COLLARED ON LAND #########
#########################################################

library(tidyverse)
library(here)
library(conflicted)

conflicts_prefer(
  dplyr::filter()
)

# ----- LOAD AND PREP DATA  --------------- #

# Load

b <- readRDS(here("Data", "Derived-data", "DFs", "OG_ch2_denInfo.Rds"))

pag <- read_csv(here("Data", "Pagano_bears.csv"))

# Prep

pag <- pag %>%
  select(`Bear ID`, Year) %>%
  mutate(`Bear ID` = as.character(`Bear ID`)) %>%
  unite("id", `Bear ID`:Year, sep = '.', remove = TRUE) %>%
  mutate(id = paste0("pb_", id))

pag[1,] <- "pb_06810.2008"

#saveRDS(pag, here("Data", "Derived-data", "Pag_IDs.Rds"))

b <- b %>% 
  select(-study_start)

ice <- b %>%
  filter(landfall == 1) 

iceIDs <- unique(ice$id)

landCollar <- b %>% 
  filter(! id %in% iceIDs) %>%
  group_by(id) %>%
  slice_head() %>%
  mutate(study_start = 1) 

b <- b %>%
  left_join(landCollar) %>% glimpse()




%>%
  mutate(study_start == 1) %>%
  mutate(landfall == 0)


