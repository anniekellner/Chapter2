##########################################################################
####    REVISE START DATES FOR PAG BEARS CAPTURED ON LAND    #############
##########################################################################

library(tidyverse)
library(here)
library(conflicted)

conflicts_prefer(
  dplyr::filter()
)

rm(list = ls())

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

# --  REVISE START DATES--------- #

# 5 calendar days after date of capture

p <- b %>% # pag bears captured on land
  filter(id == "pb_20965.2008" | id == "pb_20975.2008") %>%
  select(id, ymd) %>%
  group_by(id) %>%
  slice_head()

p$start_date <- p$ymd + ddays(5)

p <- p %>%
  select(-ymd) %>%
  rename(ymd = start_date) %>%
  left_join(b) %>%
  group_by(id) %>%
  slice_head() 

b <- b %>% # 32017 rows
  mutate(study_start = case_when(
    id == "pb_20965.2008" & datetime == '2008-08-29 00:00:00' | 
      id == "pb_20975.2008" & datetime == '2008-08-27 00:00:00' ~ 1,
    TRUE ~ study_start)) %>% glimpse()

# -- REMOVE DATES PRIOR TO STUDY START FOR PAG BEARS  ---- #
  
b <- b %>% # every variable becomes NA prior to landfall (except id which is grouping variable) # 32017
  group_by(id) %>%
  mutate(across(everything(),
                ~replace(., row_number() < match(1, study_start), NA))) %>%
  ungroup()

noID <- b %>% # Remove rows where all vars are NA but ID # 31893 rows
  select(animal:study_start) %>%
  filter(if_any(everything(), ~ !is.na(.)))

b <- b %>% # Put ID back in dataframe 
  right_join(noID) # 31893 rows: looks good

#saveRDS(b, here("Data", "Derived-data", "DFs", "OG_ch2_082623.Rds"))







