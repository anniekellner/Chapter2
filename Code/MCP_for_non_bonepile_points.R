###############################################################
###   DETERMINE MCP'S FOR NON-BONEPILE LOCATIONS    ###########
###############################################################

# Use this script in combination with bonepile_denning_info.xlsx 
# and use_nsd_to_determine_bonepile_arrival.R
# also can reference determine_bonepile_radius_using_NSD_asymptote.R

library(sf)


rm(list = ls())


# ------------------  Load Data ---------------------------- #

pb <- readRDS('./Data/bears_092921.Rds')

# --------------  Non-bonepile bears  ------------------------------------------------- #

no.bp <- pb %>%
  filter(id == "pb_20418.2005" | id == "pb_21237.2011" | id == "pb_32255.2008" | id == "pb_20414.2009") %>%
  select(id, geometry)

no_bp.sp <- as_Spatial(no.bp)

no_bp.mcp <- mcp(no_bp.sp, percent = 95) # mcp spdf

# Plot

plot(no_bp.sp, col = as.factor(no_bp.sp@data$id), pch = 16)
plot(no_bp.mcp, col = alpha(1:3, 0.5), add = TRUE)

# ------------- Bonepile bears not at bonepile  ------------ #

