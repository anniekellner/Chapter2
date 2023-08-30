###################################################################
#####   STUDY END DATES FOR ALL BEARS   ###########################
###################################################################

# End dates:
# Denning bears: the first day after having spent at least three successuve days in their den location
# Departing Bears: last land point before bears leave ice and do not use land for >= 7 days
# Other:
  # When collars stop transmitting data
  # For bears that use land through Dec 31, mean ordinal date of remaining bears

# Mean ordinal end date for denning and ice bears is 307.1 (ordinal)
  # Nov 3 in a non-leap year
  # Nov 2 in a leap year

library(tidyverse)
library(here)
library(conflicted)

conflicts_prefer(
  dplyr::filter()
)

rm(list = ls())

# --------  LOAD AND PREP DATA ------------- #

b <- readRDS(here("Data", "Derived-data", "DFs", "OG", "OG_add_depart_ice.Rds"))

# Prep

b <- select(b, -collar_drop)

b$ordinal_date <- yday(b$ymd)

# ----  ADD END DATES FOR BEARS THAT DEPART FOR ICE AND DENNING BEARS ------ #

ice <- b %>%
  filter(departure_to_ice == 1) 

den <- b %>%
  filter(enter_den == 1) 

iceDen <- b %>%
  filter(departure_to_ice == 1 | 
           enter_den == 1) 

max(iceDen$ordinal_date) # 343, Dec 8 in a regular year. 


# ------ DATA LOST IN SEPTEMBER OR OCTOBER  --------- #

iceIDs <- unique(ice$id)
denIDs <- unique(den$id)

# Likely collar malfunction

cd <- b %>%
  filter(!(id %in% iceIDs | id %in% denIDs)) %>%
  group_by(id) %>%
  slice_tail() %>%
  filter(month < 11) %>% glimpse()

cdIDs <- unique(cd$id)

# But look at 2008 bears, because may have departed for ice 
# Considering how close these bears are to the ice departure dates in 2008, possible they returned to ice
# Also likely the other October bears left for ice early as well

ice2008 <- ice %>% filter(year == 2008)
ice2008

cd2008 <- cd %>% filter(year == 2008)
cd2008

# ------- BEARS THAT REMAIN ON ICE THROUGH DECEMBER 31 ------- #

# Cut data off at ordinal date 307

land_end <- b %>%
  filter(!(id %in% iceIDs | 
             id %in% denIDs | 
             id %in% cdIDs)) %>%
  filter(ordinal_date < 344) %>%
  group_by(id) %>%
  slice_tail() 

# ----- ADD STUDY_END COLUMN FOR ALL BEARS  ------------- #

studyEnd <- bind_rows(cd, den, ice, land_end)

studyEnd$study_end <- 1

b2 <- b %>%
  left_join(studyEnd) %>%
  replace_na(list(study_end = 0))

# ---   ELIMINATE DATES AFTER STUDY_END ------------- #

b3 <- b2 %>%
  group_by(id) %>%
  mutate(across(everything(),
                ~replace(., row_number() > match(1, study_end), NA))) %>%
  ungroup()

noID <- b3 %>% # Remove rows where all vars are NA but ID # 24716 rows
  select(animal:study_end) %>%
  filter(if_any(everything(), ~ !is.na(.)))

b3<- b3 %>% # Put ID back in dataframe 
  right_join(noID) # 324716 rows: looks good

b3 %>% group_by(id) %>% slice_tail() %>% glimpse() # check - looks good

saveRDS(b3, here("Data", "Derived-data", "DFs", "OG", "OG_082823.Rds"))
