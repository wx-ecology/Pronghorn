################ Barrier Behavior Analysis (BaBA): distance from pts to fences ##############
# Time: 11272019
# Description: this script is to calculate distance from each movement point to closest fence line

# Note: this script takes a long time to run. Mostly because the distance function takes forever. There might be a better function to use.
# Alternatively, you can use "generate near table" funciton in ArcGIS for much much faster process. 

## Input: fence shp, movement points csv

#############################################################################################
# ----- set up -------
library(dplyr)
# spatial analysis
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(geosphere)
#trajectory analysis
library(adehabitatLT)
# for parallel multi-core calculation 
library(foreach)
library(doParallel)
library(doSNOW)
# plot
library(ggplot2)

setwd("C:\\Users\\wenjing.xu\\Google Drive\\RESEARCH\\Pronghorn\\Analysis\\FenceBehavior_Official")

#############################
#########Parameters##########
#############################
target.crs <- "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"


#############################
######### Set-up ###########
#############################
# prepare spatial dataframe -----------------------------------
#read in fence data
fence.filename <- 'Fence_convex_FINAL'
fence.sp <- readOGR(".", fence.filename)
fence.sp.ll <- spTransform(fence.sp, CRS("+proj=longlat"))

#read in movement data
movement.df.all <- read.csv("Int2_Comp_Raw_All.csv")  
movement.df.all$date <- as.POSIXct(strptime(as.character(movement.df.all$date),"%m/%d/%Y %H:%M")) #change the format based on the data
movement.df.all <- movement.df.all[(!is.na(movement.df.all$date))&(!is.na(movement.df.all$Easting)),]

# add point ID by individual if there has no such column
movement.df.all$ptsID <- numeric(nrow(movement.df.all))
for (i in unique(movement.df.all$Location.ID)) {
  mov.seg.i <- movement.df.all[movement.df.all$Location.ID==i,]
  movement.df.all[movement.df.all$Location.ID==i,]$ptsID <- seq(nrow(mov.seg.i))
}

# make movement pts into spatial point dataframe
xy <- cbind(movement.df.all$Easting, movement.df.all$Northing)
#movement.traj <- as.ltraj(xy = xy, date = movement.df.all$date, id = movement.df.all$Location.ID, proj4string = CRS(target.crs)) 
movement.sp.all <- SpatialPointsDataFrame (coords = xy, data = movement.df.all, proj4string = CRS(target.crs))
mov.sp.all.ll <- spTransform(movement.sp.all, CRS("+proj=longlat"))
mov.sp.all.ll$Longitude <- coordinates(mov.sp.all.ll)[,1]
mov.sp.all.ll$Latitude <- coordinates(mov.sp.all.ll)[,2]
movement.df.all.ll <- as.data.frame(mov.sp.all.ll@data)

#############################
######## Analysis ###########
#############################

# for parallel looping 
cores <- detectCores()
cl <- makeSOCKcluster(cores[1]-1) #to not overload your computer
#registerDoParallel(cl)
registerDoSNOW(cl)

pb <- txtProgressBar(max=100, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)


#start the loop
distance.df = foreach (ii = unique(movement.df.all.ll$Location.ID), 
                    .combine=rbind,
                    .options.snow=opts,
                    .packages=c('raster', 'sp', 'rgdal', 'rgeos', 'adehabitatLT', 'sp', 'dplyr', 'geosphere')) %dopar% 
  {   
      movement.df.ii <-movement.df.all.ll[movement.df.all.ll$Location.ID == ii, ]
      
      distance.df.ii <- data.frame(AnimalID = character(), x = numeric(), y = numeric(), dist2fence = numeric())
      
      for (iii in seq(1, nrow(movement.df.ii), by = (10))) {  # number of points skip if you have a lot of movement points
        
        cpt_x_iii <- movement.df.ii$Longitude[iii]
        cpt_y_iii <- movement.df.ii$Latitude[iii]
        
        cpt <- c(cpt_x_iii, cpt_y_iii)
        dist2fence.iii <- dist2Line(cpt, fence.sp.ll)
        
        distance.df.iii <- cbind(AnimalID = ii, x = cpt_x_iii, y = cpt_y_iii, dist2fence = dist2fence.iii[1,1]) 
        distance.df.ii <- rbind(distance.df.ii, distance.df.iii)
      }
    distance.df.ii
  }

write.csv(distance.df, "I2_all_Dist2Fence.csv")

close(pb)
#stop cluster
stopCluster(cl)


# ------ Check distribution of the calculated distances ---------
distance.df <- read.csv("I2_all_Dist2Fence.csv")
qu1 <-as.numeric(summary(distance.df$dist2fence)[2])
qu2 <-as.numeric(summary(distance.df$dist2fence)[3])

plot.distden <- ggplot(distance.df, aes(x = dist2fence)) +
  geom_density(color = '#EA992E', size = 1.5) + 
  xlim(0, 6000) +
  geom_vline(xintercept = qu1, color = '#ea4d2e', size = 1) +
  geom_vline(aes(xintercept= qu2, color="ea4d2e"),linetype="dashed", size = 1) +
  labs (title = "Distribution of distances from pronghorn locations during migration to fences (smoothed)", x = "Density", y = "Distance to Fences (m)") +
  theme_bw() + 
  theme(legend.position = "none") 
plot.distden

plot.distden + 
  geom_text(aes(x = qu1, y = 0.0003, 
                label = paste0("1st Quantile =", round(qu1, digits = 2), "m")), angle=90, vjust = 1.3, size = 4) +
  geom_text(aes(x = qu2, y = 0.0003, 
                label = paste0("2nd Quantile =", round(qu2, digits = 2), "m")), angle=90, vjust = 1.3, size = 4) 

