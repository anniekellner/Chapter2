#########################################################
###   DOES THE TYPE OF MUNICIPALITY MATTER?   ###########
#########################################################

# Do bears avoid some types of municipal places but not others? 
# Conduct an iSSA to see whether bears avoid some types of munipalities and not others

# Different types of municipal layers:
# North Slope Villages = city centers for Kaktovik, Deadhorse, etc. 
# Native Lands NPRA - Native territory within NPRA
# Native Allotments_NS - individuallyowned Native-held lands
# Cabins - hunting/fishing camp cabins for subsistence activities
# Village BND - municipal boundaries of Kaktovik, Barrow, Niuqsut, etc. 

rm(list = ls())

library(sf)
library(dplyr)

# --------  DATA ----------------------   #

# Directories

NSB <- './Data/Spatial/NSB'

corr <- readRDS('./Data/Derived-data/corridor_data.Rds')

# Villages

vill <- st_read(paste(NSB, 'North Slope Villages.shp', sep = '/'))

# Native Allotments

na1 <- st_read('./Data/Spatial/NSB/Native_Allotments_NS.shp')
na2 <- 
  
  # Native lands
  
  native_muni <- st_read('./Data/Spatial/NSB/Native_lands_NPRA.shp')
