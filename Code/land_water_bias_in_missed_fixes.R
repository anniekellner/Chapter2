########################################################################
##  Is there a bias in which water points are not being recorded? ######
########################################################################

# Compare results from KS test with skipped fix rate. 
# Are bears with non-random skipped fixes the same bears that have high percentage of missed fixes?
# Only 2 bears have both high rates of skipped fixes AND non-random skipped fixes
# Based on these results, going to proceed without worrying about points in water

rm(list = ls())

# Load data

ks <- readRDS('./Results/ks_results.Rds')
akde <- readRDS('./Data/akde_df.Rds')

# Cross-reference bears with high percentage of missing fixes and non-random missing fixes

# Missing
missing <- filter(akde, missed_fixes_pct > 20)
missing_ids <- missing$id

# Non-random

non_random <- filter(ks, p.value > 0.05)
non_random_ids <- non_random$id

intersect(missing_ids, non_random_ids)

# results: only two bears had >20% fixes missing AND had non-random missing fixes