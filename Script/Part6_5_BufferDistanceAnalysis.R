################ What do animals do in face of barriers ######################################
# Time of first creation: 10312019
## Input: movement points csv (with the same time intervals since the behavior type assumption 
# is related to how much time is represented between two movement points; coordinates are named Easting/Northing (in m not degree))
## Goal: this script is to examine best buffer distance for follwing behavior classifications. 
## figure: x-axis - distance to fence; y-axis: segment straightness, group by segmentation duration for stragithess (number of points used to calculate straightness)

# current types are: bounce, trace, back and forth, 
# trapped (could also be a home range behavior), none (not a barrier response), and TBD (hard to classify)

# update: calculating moving window straightness in order to calculate proper cut-offs for back-n-forth and trace

# This 10 individual trial is to explore ways to quantitatively select straightness cut-off. 
# In addition, testing impact of different time intervals by resampling 2h data into 4h and 6h

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

setwd("C:\\Users\\wenjing.xu\\Google Drive\\RESEARCH\\Pronghorn\\Analysis\\FenceBehavior")

#############################
#########Parameters##########
#############################
target.crs <- "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#############################
#########Functions###########
#############################
# calculating straightness. Input a dataframe with Easting and Northing. 
strtns <- function(mov.seg) {
  pts <- cbind(mov.seg$Easting, mov.seg$Northing)
  pts.sp <- SpatialPointsDataFrame(pts, mov.seg, proj4string = CRS(target.crs))
  traj <- as.ltraj(xy =  pts, date = mov.seg$date, id = mov.seg$Location.ID)
  #moving distance from first pt to last pt in the burst
  traj.dist <- sqrt(
    as.numeric((traj[[1]]$x[1]-traj[[1]]$x[nrow(traj[[1]])]))*  as.numeric((traj[[1]]$x[1]-traj[[1]]$x[nrow(traj[[1]])])) +
      as.numeric((traj[[1]]$y[1]-traj[[1]]$y[nrow(traj[[1]])]))*as.numeric((traj[[1]]$y[1]-traj[[1]]$y[nrow(traj[[1]])])) 
  )
  #sum of all step lengths
  traj.lgth <- sum(traj[[1]]$dist, na.rm = TRUE)
  #straightness ranges from 0 to 1. More close to 0 more sinuous it is.
  straightness <- traj.dist/traj.lgth
  return(straightness)
}

#############################
######### Set-up ###########
#############################
# prepare spatial dataframe -----------------------------------
#read in fence data
fence.filename <- 'Fence062219_AOI_prjd'
fence.sp <- readOGR(".", fence.filename)
#fence.sp <- spTransform(fence.sp,target.crs)
fence.sp.ll <- spTransform(fence.sp, CRS("+proj=longlat"))

#read in movement data
#ideally, the movement data should not have missing point. This trial file does have missing points.
movement.df.all <- read.csv("DataTable/pinedale.int2.raw.10ind.AprOct.csv") 
movement.df.all$date <- as.POSIXct(strptime(as.character(movement.df.all$date),"%m/%d/%Y %H:%M")) #change the format based on the data
#ideally there should be the less NA rows the better 
movement.df.all <- movement.df.all[!is.na(movement.df.all$date),]

# add point ID by individual
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

# set an empty dataframe
distance.df <- data.frame(AnimalID = character(), duration = numeric(), straightness = numeric(), cpt_x = numeric(), cpt_y = numeric(), dist2fence = numeric())

#start the loop
distance.df = foreach (i = c(3,6,9,12,15,18,21),  # number of points to iterate as duration to calculate straightness. 6h - 48h.
                    .combine=rbind,
                    .options.snow=opts,
                    .packages=c('raster', 'sp', 'rgdal', 'rgeos', 'adehabitatLT', 'sp', 'dplyr', 'geosphere')) %dopar% 
  {   
   
    distance.df.i <- data.frame(AnimalID = character(), duration = numeric(), straightness = numeric(),  cpt_x = numeric(), cpt_y = numeric(), dist2fence = numeric()) 
    
    for (ii in unique(movement.df.all.ll$Location.ID)) {
      
      movement.df.ii <-movement.df.all.ll[movement.df.all.ll$Location.ID == ii, ]
      Duration.ii <- i*2
      
      straightness.ii <- numeric()
      dist2fence.ii <- numeric()
      cpt_x_ii <- numeric()
      cpt_y_ii <- numeric()
      
      for (iii in seq(1,(nrow(movement.df.ii) - i + 1), by = (i*4))) {
        movement.df.iii <- movement.df.ii[(iii: (iii + i-1)), ]
        
        pts <- cbind(movement.df.iii$Longitude, movement.df.iii$Latitude)
        # cpt_x_iii <- centroid(pts)[1]
        # cpt_y_iii <- centroid(pts)[2]
        # cpt <- c(cpt_x_iii, cpt_y_iii)
        cpt_x_iii <- mean(pts[,1])
        cpt_y_iii <- mean(pts[,2])
        cpt <- c(cpt_x_iii, cpt_y_iii)
        
        if (sum(is.na(cpt)) > 0) {
          next
        }
        
        dist2fence.iii <- dist2Line(cpt, fence.sp.ll)[1]
        straightness.iii <- strtns(movement.df.iii)
        
        if ((is.na(straightness.iii)) | (is.na(dist2fence.iii))) {
          next
        }
        
        cpt_x_ii <- c(cpt_x_ii, cpt_x_iii)
        cpt_y_ii <- c(cpt_y_ii, cpt_y_iii)
        straightness.ii <- c(straightness.ii, straightness.iii)
        dist2fence.ii <- c(dist2fence.ii, dist2fence.iii)
      }
      distance.df.ii <- cbind(AnimalID = rep(ii, length(straightness.ii)), duration = rep(Duration.ii, length(straightness.ii)), staightness = straightness.ii,  cpt_x = cpt_x_ii, cpt_y = cpt_y_ii, dist2fence = dist2fence.ii) 
      distance.df.i <- rbind(distance.df.i, distance.df.ii)
    }
    distance.df.i
  }

write.csv(distance.df, file = (paste0(getwd(), "/DataTable/DistanceAnalysis/I2_4to48.csv")))

close(pb)
#stop cluster
stopCluster(cl)

# ########## test ############
# 
# # for parallel looping 
# cores <- detectCores()
# cl <- makeSOCKcluster(cores[1]-1) #to not overload your computer
# #registerDoParallel(cl)
# registerDoSNOW(cl)
# 
# pb <- txtProgressBar(max=100, style=3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress=progress)
# 
# # set an empty dataframe
# distance.df <- data.frame(AnimalID = character(), duration = numeric(), straightness = numeric(), cpt_x = numeric(), cpt_y = numeric())
# 
# #start the loop
# distance.df = foreach (i = c(3,6,9,12,15,18,21),  # number of points to iterate as duration to calculate straightness. 6h - 48h.
#                        .combine=rbind,
#                        .options.snow=opts,
#                        .packages=c('sp', 'rgdal', 'rgeos','sp', 'adehabitatLT')) %dopar% 
#   {   
#     
#     distance.df.i <- data.frame(AnimalID = character(), duration = numeric(), straightness = numeric(), cpt_x = numeric(), cpt_y = numeric()) # dist2fence = numeric()
#     
#     for (ii in unique(movement.df.all.ll$Location.ID)) {
#       
#       movement.df.ii <-movement.df.all.ll[movement.df.all.ll$Location.ID == ii, ]
#       #movement.df <- as.data.frame(movement.sp)    
#       Duration.ii <- i*2
#       
#       #cpts <- data.frame()
#       straightness.ii <- numeric()
#       #dist2fence.ii <- numeric()
#       
#       cpt_x_ii <- numeric()
#       cpt_y_ii <- numeric()
#       
#       for (iii in seq(1,(nrow(movement.df.ii) - i + 1), by = (i*4))) {
#         movement.df.iii <- movement.df.ii[(iii: (iii + i-1)), ]
#         
#         pts <- cbind(movement.df.iii$Longitude, movement.df.iii$Latitude)
#         cpt_x_iii <- mean(pts[,1])
#         cpt_y_iii <- mean(pts[,2])
#         cpt <- c(cpt_x_iii, cpt_y_iii)
#         
#         #dist2fence.iii <- dist2Line(cpt, fence.sp.ll)[1]
#         
#         straightness.iii <- strtns(movement.df.iii)
#         
#         # if (is.na(straightness.iii)) {
#         #   next
#         # }
#         
#         cpt_x_ii <- c(cpt_x_ii, cpt_x_iii)
#         cpt_y_ii <- c(cpt_y_ii, cpt_y_iii)
#         
#         straightness.ii <- c(straightness.ii, straightness.iii)
#         
#         #dist2fence.ii <- c(dist2fence.ii, dist2fence.iii)
#       }
#       distance.df.ii <- cbind(AnimalID = rep(ii, length(straightness.ii)), duration = rep(Duration.ii, length(straightness.ii)), staightness = straightness.ii, cpt_x = cpt_x_ii, cpt_y = cpt_y_ii ) #dist2fence = dist2fence.ii
#       distance.df.i <- rbind(distance.df.i, distance.df.ii)
#     }
#     distance.df.i
#   }
# 
# write.csv(distance.df, file = (paste0(getwd(), "/DataTable/DistanceAnalysis/I2_4to48test.csv")))
# 
# close(pb)
# #stop cluster
# stopCluster(cl)