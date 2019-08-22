##### STEP 3: output them as shapefile (one by one, so that I can inspect them in Arc) 
# and some simple parameter explore
# Created: Jan 2019

# Output: Analysis/Shapefiles  (Pts)
# In Arc generated tracks for making maps.

# Set up -----
library(adehabitatLT)
library(lubridate)
library(raster)
library(rworldmap)
library(sp)
library(rgdal)

setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/Shapefiles")
#### for JMH -------
prongs <- read.csv(file.choose())   #pick JMH 
prongs$time <- as.POSIXct(strptime(as.character(prongs$time), "%m/%d/%y %H:%M"))

prongs <- prongs[!is.na(prongs$Easting),]
coordinates(prongs) <- c("Easting", "Northing")
proj4string(prongs) <- CRS("+init=epsg:32612")

prongs.traj <- as.ltraj(coordinates(prongs), date = prongs$time, id = prongs$Location.ID)

#regularize data
#use the earliest time as reference. Programmed in 3 hour interval. (my whole step 1 was a waste TAT)
prongs.NA <- setNA(prongs.traj,min(prongs$time),3,units="hour") 
prongs.traj <- sett0(prongs.NA,min(prongs$time),3,units="hour")
#is.regular(prongs.traj)

plot.ltraj(prongs.traj)

# out put them as shp

for (i in 1:length(prongs.traj)) {
  prong <- ltraj2spdf(prongs.traj[i])
  prong@proj4string <- CRS("+init=epsg:32612")  #give projection WGS84 Zone 12N
  writeOGR(prong, dsn = ".", layer = paste0("JMH_",summary(prongs.traj[i])$id), driver = "ESRI Shapefile", overwrite = TRUE)
}


#### for PAPO -------
# PAPO has three different time intervals, therefore need to seperate them into three different csv. 
prongs <- read.csv(file.choose())   #pick data/dataforanalysis/PAPO_mig_cyc
prongs$time <- as.POSIXct(strptime(as.character(prongs$time), "%m/%d/%y %H:%M",tz = "MST"))
prongs <- prongs[!is.na(prongs$Easting),]

PAPO_35 <- prongs[prongs$Interval == 3.5,]
PAPO_30 <- prongs[prongs$Interval == 3,]
PAPO_20 <- prongs[prongs$Interval == 2,]  

output_individual_pts <- function (dataframe, interval) {
  coordinates(dataframe) <- c("Easting", "Northing")
  proj4string(dataframe) <- CRS("+init=epsg:32612")
  dataframe$Location.ID <- droplevels(dataframe$Location.ID)
  dataframe.traj <- as.ltraj(coordinates(dataframe), date = dataframe$time, id = dataframe$Location.ID)
  #regularize data
  dataframe.NA <- setNA(dataframe.traj,min(dataframe$time),interval,units="hour")
  dataframe.traj <- sett0(dataframe.NA,min(dataframe$time),interval,units="hour")

  # out put them as shp
  for (i in 1:length(dataframe.traj)) {
    prong <- ltraj2spdf(dataframe.traj[i])
    prong@proj4string <- CRS("+init=epsg:32612")  #give projection WGS84 Zone 12N
    writeOGR(prong, dsn = ".", layer = paste0("PAPO_",summary(dataframe.traj[i])$id), driver = "ESRI Shapefile", overwrite = TRUE)
  }
}

output_individual_pts(PAPO_35, 3.5)
output_individual_pts(PAPO_30, 3)
output_individual_pts(PAPO_20, 2)   

## convert traj to dataframe
#prongs.df <- ld(prongs.traj)
