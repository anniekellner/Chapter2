#########################################
#####   ADD REPRO DATA TO DATAFRAME #####
#########################################

library(dplyr)

rm(list = ls())

# ------ Load and prep data  -------------- #

pb <- readRDS('./Data/Derived-data/DFs/bears_ch2_091922.Rds')
ch1 <- readRDS('C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/ch1_landing/data/derived-data/all.Rds') # Different dataset from Ch1

repro <- ch1 %>% 
  select(animal, year, id, repro) %>%
  group_by(id) %>%
  slice_head()

ids <- unique(pb$id)

repro2 <- repro %>%
  filter(id %in% ids) %>%
  select(id, repro)

# ---- Join ------------------------------- #

pb2 <- pb %>%
  full_join(repro2)

pb2 %>%
  group_by(id) %>%
  slice_head() %>%
  select(id, animal, repro, age) %>%
  print(n = 21)

# ---   Look into missing data for 20418 ---------------- #

ch1 %>% 
  select(animal, year, id, repro) %>%
  group_by(animal) %>%
  slice_head() %>%
  filter(animal == "pb_20418")

# In 2004, pb_20418 had a coy. Missing data is for 2005. Possibly had a yearling. 
# Adding this repro data to dataframe, but need to check to see what is extrapolated and what is not. 

# Add extrapolated data to 20418

repro2[4,2] <- "yearling"

# Then re-run lines 27 & 28 

# Save repro data but need to figure out what is extrapolated 
  
saveRDS(pb2, 'Data/Derived-data/DFs/bears_ch2_091922.Rds')

