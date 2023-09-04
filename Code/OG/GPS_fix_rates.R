############################################################
######    GPS DATA - FIX RATES  ############################
############################################################

# See Space_use_summaries.Rmd for additional code

library(tidyverse)
library(adehabitatLT)
library(here)
library(conflicted)

conflicts_prefer(
  dplyr::filter(),
  dplyr::select()
)

# ------- LOAD AND PREP DATA  ----------------  #

b <- readRDS(here("Data", "Derived-data", "DFs", "OG", "OG_090323.Rds"))
b <- as.data.frame(b)

# ----  CREATE TRAJ OBJECT  --------------- #

ltraj <- as.ltraj(xy=b[,c("Xaa","Yaa")], date=b$datetime, id=as.character(b$id))

trajDF <- ld(ltraj)

trajDF <- trajDF %>%
  mutate(fix_rate=dt/3600)

fixes <- trajDF %>% # all values for missing_fixes are 0 and fix.pct = Inf
  group_by(id) %>%
  summarise(n.fixes = n(),
            missing_fixes = sum(is.na(date)),
            fix.pct = n.fixes/missing_fixes)

# Average fix rates

avgs <- trajDF %>%
  group_by(id) %>%
  na.omit() %>%
  summarise(mean = mean(fix_rate), median = median(fix_rate)) 

# Create table and save

avgs <- avgs %>%
  select(-mean) %>%
  arrange(median)

#saveRDS(avgs, here("Data", "Derived-data", "DFs", "OG", "Fix_Rates.Rds"))

# ---------  MISSED FIXES ------------------  #

avgs$median <- round(avgs$median, digits = 0)

fix1 <- avgs %>% filter(median == 1) %>% select(id)
fix1$id -> fix1IDs

fix2 <- avgs %>% filter(median == 2) %>% select(id)
fix2$id -> fix2IDs 

fix4 <- avgs %>% filter(median == 4) %>% select(id)
fix4$id -> fix4IDs

# Filter into groups by fix rate

one <- filter(trajDF, id %in% fix1IDs)
two <- filter(trajDF, id %in% fix2IDs)
four <- filter(trajDF, id %in% fix4IDs)

# Create separate trajectories for each fix rate

traj.one <- as.ltraj(xy = one[,c('x','y')], date = one$date, id = one$id)
traj.two <- as.ltraj(xy = two[,c('x','y')], date = two$date, id = two$id)
traj.four <- as.ltraj(xy = four[,c('x','y')], date = four$date, id = four$id)


# SetNAs

refda <- parse_date_time(paste(min(b$datetime)), orders = 'ymd HMS', tz = 'US/Alaska')

# Nearly regular (times may still vary slightly)

NA1 <- setNA(traj.one, refda, 1, units = "hour")
NA2 <- setNA(traj.two, refda, 2, units = "hour")
NA4 <- setNA(traj.four, refda, 4, units = "hour")

# Regular: accounting for small GPS inconsistencies

na1 <- sett0(NA1, refda, 1, units = "hour")
na2 <- sett0(NA2, refda, 2, units = "hour")
na4 <- sett0(NA4, refda, 4, units = "hour")


hrs <- "dt/3600" # so don't have to keep typing this out

# Convert to summary df so can manipulate data (cannot manipulate data when it's an ltraj object)

sum1 <- summary(na1)
sum2 <- summary(na2)
sum4 <- summary(na4)

sum1 <- sum1 %>%
  mutate(fix_rate = "1 hour")

sum2 <- sum2 %>%
  mutate(fix_rate = "2 hours")

sum4 <- sum4 %>%
  mutate(fix_rate = "4 hours")

all_sums <- rbind(sum1, sum2, sum4)

all_sums <- all_sums %>%
  mutate(missed_fixes_pct = (NAs/(nb.reloc + NAs)*100)) %>%
  dplyr::select(id, missed_fixes_pct)

summary(all_sums$missed_fixes_pct)
sd(all_sums$missed_fixes_pct)
