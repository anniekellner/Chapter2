############################################################
######    GPS DATA - FIX RATES  ############################
############################################################


library(tidyverse)
library(adehabitatLT)
library(here)
library(conflicted)



traj2 <- ld(traj.pb)

traj2 <- traj2 %>%
  mutate(fix_rate=dt/3600)

fixes <- traj2 %>% 
  group_by(id) %>%
  summarise(n.fixes = n(),
            missing_fixes = sum(is.na(date)),
            fix.pct = n.fixes/missing_fixes)
fixes
