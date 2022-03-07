#########################################################
###   DOES THE TYPE OF MUNICIPALITY MATTER?   ###########
#########################################################

# Do bears avoid some types of municipal places but not others? 
# Conduct an iSSA to see whether bears avoid some types of munipalities and not others

# Different types of municipal layers:
# North Slope Villages = city centers for Kaktovik, Deadhorse, etc. 
# Native Lands NPRA - Native territory within NPRA
# Native Allotments_NS - individually owned Native-held lands
# Cabins - hunting/fishing camp cabins for subsistence activities
# Village BND - municipal boundaries of Kaktovik, Barrow, Niuqsut, etc. 

rm(list = ls())

library(sf)
library(dplyr)

# --------  DATA ----------------------   #

# Directories

NSB <- './Data/Spatial/Municipal/NSB'
NLB <- './Data/Spatial/Municipal/Native lands & boundaries'

corr <- readRDS('./Data/Derived-data/corridor_data.Rds')

# Villages

vill <- st_read('./Data/Spatial/Municipal/NSB/North Slope Villages.shp') # File not working - need to fix

# Native Allotments

na1 <- st_read('./Data/Spatial/Municipal/NSB/Native_Allotments_NS.shp')
na2 <- st_read(paste(NLB, "native_allotments_east.shp", sep = '/'))

native_allot <- st_join(na1, na2)
  
# Native lands
  
native_muni <- st_read('./Data/Spatial/NSB/Native_lands_NPRA.shp')

# Cabins

cabins <- st_read(paste(NLB, 'native_cabins.shp', sep = '/'))
