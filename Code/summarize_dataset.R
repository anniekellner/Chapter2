####################################################
##    Summarize dataset       ######################
#####################################################

library(dplyr)

rm(list = ls())

akde <- readRDS('./Data/akde_df.Rds')

# Fix rates

ones <- filter(akde, median_fix == 1)
twos <- filter(akde, median_fix == 2)
fours <- filter(akde, median_fix == 4)
eight <- filter(akde, median_fix == 8)

# Missed fixes

mean(akde$missed_fixes_pct)
min(akde$missed_fixes_pct)
max(akde$missed_fixes_pct)
sd(akde$missed_fixes_pct)
