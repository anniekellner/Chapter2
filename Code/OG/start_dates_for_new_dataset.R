########################################################################
##########    DETERMINE START DATES FOR NEW DATASET  ###################
########################################################################

# August 2023
# Include Pagano bears and new start dates for landfall

library(tidyverse)
library(sf)
library(conflicted)

conflicts_prefer(
  dplyr::filter()
)

rm(list = ls())

# ------ LOAD AND PREP DATA  ------------------------------- #

ch2ice <- readRDS("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/Chapter2/Data/Derived-data/DFs/ch2_bears_with_Pag_all_days.Rds")

orig_Ch2 <- readRDS("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/Chapter2/Data/Derived-data/DFs/bears_ch2_052823.Rds")

iceIDs <- unique(ch2ice$id)

landCollar <- orig_Ch2 %>%
  filter(!id %in% iceIDs)

landIDs <- unique(landCollar$id)

ice <- ch2ice %>% # remove land bears from dataset
  filter(!id %in% landIDs)

# ---- ADD START_DATE COLUMN FOR ICE BEARS AND REMOVE PREVIOUS DATES ---------------------- #

# Original start dates for comparison

orig_Ch2_start <- filter(orig_Ch2, study_start == 1) 

# New start dates

ice <- ice %>%
  mutate(study_start = if_else(landfall == 1, 1, 0)) # 18 bears with start_date at landfall 

ice2 <- ice %>% # every variable becomes NA prior to landfall (except id which is grouping variable)
  group_by(id) %>%
  mutate(across(everything(),
                ~replace(., row_number() < match(1, study_start), NA))) %>%
  ungroup()

noID<- ice2 %>% # Remove rows where all vars are NA but ID
  select(animal:study_start) %>%
  filter(if_any(everything(), ~ !is.na(.)))

ice3 <- ice2 %>% # Put ID back in dataframe 
  right_join(noID)

ch2 <- full_join(ice3, landCollar)

saveRDS(ch2, file = 'Data/Derived-data/DFs/ch2_no_end_cutoff_080823.Rds')

# -------   CHECK IDS AGAINST PAGANO BEARS  --------------- #

pag <- read_csv('./Data/Pagano_bears.csv')

pagIDs <- pag$ID # 25 bears

# alter ways IDs are written to accommodate Pagano csv style

ch2_bears <- ch2 %>% # 28 bears
  group_by(animal, year) %>%
  slice_head() %>%
  separate_wider_delim(animal, delim = "_", names = c(NA, "ID"), cols_remove = TRUE) %>%
  select(ID, year) %>%
  mutate(ID = as.double(ID)) %>%
  rename(YEAR = year) 

setdiff(pag, ch2_bears) # 3 bears more in my dataset; all bears included from Pagano et al. 
  