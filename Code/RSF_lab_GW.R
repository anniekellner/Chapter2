# This code examines habitat selection patterns of deer using a resource selection function
# The input files are GPS locations from collared deer in the Piceance Basin and remote sensing data
# Any line that is preceded by # is a commented line, meaning R will not recognize it.
# Thus you should not enter these lines into R, rather they are provided for organizational means
# If you make an error or need to start over you can type: rm(list=ls()) to clear the memory from R
# and start over from the beginning.

# Remove anything in memory
rm(list=ls())

setwd("C:/Users/akell/Dropbox/MovementEcology/GIS Lab5 RSF Basic")

# Load needed libraries

library(arm)
library(sp)
library(raster)

# # # # # Load data # # # # # 

deer.locs<-read.csv(file.choose())	#navigate to deer.csv file

elevation<-raster(file.choose())	#navigate to elevation file

wells<-raster(file.choose())		#navigate to wells file

shrub<-raster(file.choose())		#navigate to shrub file

barren<-raster(file.choose())		#navigate to barren file

rds<-raster(file.choose())			#navigate to rds file

mcp<-read.csv(file.choose())		#navigate to home_range.csv file

deer<-SpatialPoints(deer.locs)


# # # # # Plot and examine data # # # # # 

# Plot elevation 

plot(elevation)

# Add deer locations

points(deer)

# Plot wells- number of wells within 800 m 

plot(wells*1130973)

# Add deer locations

points(deer)

# Plot shrubs

plot(shrub)

# Add deer locations

points(deer)

# Plot barren

plot(barren)

# Add deer locations

points(deer)

# Plot distance to roads

plot(rds)

# Add deer locations

points(deer)


# # # # # Extract data # # # # # 

# Create empty dataset

used=matrix(1, length(deer),6)

# Intersect deer locations with habitat layers and store in empty dataset
#You can visualize populating this dataset as you step through the following

used[,2]=extract(elevation, deer)
used[,3]=extract(wells, deer)
used[,4]=ceiling(extract(shrub, deer))
used[,5]=ceiling(extract(barren, deer))
used[,6]=extract(rds, deer)
used[,3]=round(used[,3]*1130973)
used=as.data.frame(used)
names(used)=c("Used","Elevation","Wells", "Shrub", "Barren", "Roads")


# Look at data 

used

# # # # # Create random data # # # # # 

random=spsample(Polygon(mcp), 1000, "random")

# Plot elevation and MCP with random and used data

plot(elevation)

polygon(mcp, lwd=2)

points(deer, pch=16, col="red")

points(random)

# Create empty dataset

rand=matrix(0, 1000,6)

# Intersect random locations with habitat layers and store in empty dataset

rand[,2]=extract(elevation, random)
rand[,3]=extract(wells, random)
rand[,4]=ceiling(extract(shrub, random))
rand[,5]=ceiling(extract(barren, random))
rand[,6]=extract(rds, random)
rand[,3]=round(rand[,3]*1130973)
rand=as.data.frame(rand)
names(rand)=c("Used","Elevation","Wells", "Shrub", "Barren", "Roads")


# # # # # Analyze data # # # # # 

# Combine used and random data

all.data=rbind(used, rand)

attach(all.data)

#See question 1

# Run model 1- elevation, shrub, barren

model1<-glm(Used~Elevation+Shrub+Barren, family=binomial(link="logit"))

summary(model1)

#See Question 2

# Run model 2- wells and roads

model2<-glm(Used~Wells+Roads, family=binomial(link="logit"))

summary(model2)

#See Question 3

# Run model 3- all variables

model3<-glm(Used~Elevation+Wells+Shrub+Barren+Roads, family=binomial(link="logit"))

summary(model3)

#Question 4

# # # # # Make predictive Surface # # # # # 

detach(all.data)

#We are going to make a predictive surface from your regression model results. 
#Enter the coefficients from your model into the predictive function below:

rsf<-exp(wells*-0.01+shrub*0.5+barren*0.8+rds*0.5)

plot


# Add deer locations

points(deer)

# Predictive surface on subset of area

# Subset rasters

sub=extent(723917.5,726846.9,4415214,4425294 )

elevation_1=crop(elevation,sub)

shrub_1=crop(shrub,sub)

barren_1=crop(barren,sub)

rds_1=crop(rds,sub)

wells_1=crop(wells,sub)

rsf<-exp(wells_1*-0.01+shrub_1*0.5-barren_1*0.8+rds_1*0.0005)

plot(rsf)

# Add deer locations

points(deer)


