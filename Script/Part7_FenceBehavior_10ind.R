################ What do animals do in face of barriers ######################################
# Time of first creation: 10252019
## Input: Fence shp, movement points (with the same time intervals since the behavior type assumption 
# is related to how much time is represented between two movement points; coordinates are named Easting/Northing (in m not degree))
## output: a table with animal ID, encounter event ID (indicating starting time YYMMDDHH), # of movement points, straightness index (if calculated), classified type

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
#trajectory analysis
library(adehabitatLT)
# for parallel multi-core calculation 
library(foreach)
library(doParallel)
library(doSNOW)


setwd("C:\\Users\\wenjing.xu\\Google Drive\\RESEARCH\\Pronghorn\\Analysis\\FenceBehavior")
#setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/FenceBehavior")

#############################
#########Parameters##########
#############################
target.crs <- "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
# ----- costamized parameter 1 --------------------------
interval <- 2 #define time interval (in hours) of the movement data
# ----- costamized parameter 2 --------------------------
# Fence buffer distance in meters 
FB.dist <- 500
# ----- costamized parameter 3 --------------------------
# tolerance parameter. If "a" is point in the buffer, "b" is a buffer outside of the buffer
# x is the number of b that is allowed in between of a to allow the point series be considered as a continuous encounter event
# maybe useful for high temporal resolution data
tolerance <- 0
# ------ costomized parameter 4 ------------------------
# based on the data time interval and animal ecology, maximum # of points in buffer that you'd call it a "bounce". 
# more than this parameter, it would be back-n-forth or along or trap.
# should be adjusted based on the temporal resolution of the input movement data
b <- 2
# minimum # of points in the burst that you'd call it a trap
p <- 24

# for parallel looping 
cores <- detectCores()
cl <- makeSOCKcluster(cores[1]-1) #to not overload your computer
#registerDoParallel(cl)
registerDoSNOW(cl)

#############################
#########Functions###########
#############################
movement.segment.b <- function(m) { # extract movement segment between the time of ot m and predetermined time lap b*interval.  
  n <- m + b # if there are missing datapoint, this n point might be over the b*interval that it should be.
  segments <- movement.df[which(movement.df$ptsID >= m & movement.df$ptsID <=n),]
  seg.line <- Lines(Line(cbind(segments$coords.x1, segments$coords.x2)), ID = segments$date[1])
  segments.sp <- SpatialLines(list(seg.line), proj4string = CRS(target.crs))
  return(segments.sp)
}

# calculating straightness. Input a dataframe with Easting and Northing. 
strtns <- function(mov.seg) {
  pts <- cbind(mov.seg$Easting, mov.seg$Northing)
  pts.sp <- SpatialPointsDataFrame(pts, mov.seg, proj4string = CRS(target.crs))
  traj <- as.ltraj(xy =  pts, date = mov.seg$date, id = mov.seg$Location.ID)
  #moving distance from first pt to last pt in the burst
  traj.dist <- sqrt(
    (traj[[1]]$x[1]-traj[[1]]$x[nrow(traj[[1]])])*(traj[[1]]$x[1]-traj[[1]]$x[nrow(traj[[1]])]) +
      (traj[[1]]$y[1]-traj[[1]]$y[nrow(traj[[1]])])*(traj[[1]]$y[1]-traj[[1]]$y[nrow(traj[[1]])]) 
  )
  #sum of all step lengths
  traj.lgth <- sum(traj[[1]]$dist, na.rm = TRUE)
  #straightness ranges from 0 to 1. More close to 0 more sinuous it is.
  straightness <- traj.dist/traj.lgth
  return(straightness)
}

# Log <- function(text, ...) {
#   msg <- sprintf(paste0(as.character(Sys.time()), ": ", text, "\n"), ...)
#   cat(msg)
#   write.socket(log.socket, msg)
# }

# prepare dataframe -----------------------------------
#read in fence data
fence.filename <- 'Fence062219_prjd'
fence.sp <- readOGR(".", fence.filename)
#fence.sp <- spTransform(fence.sp,target.crs)
fence.buffer <- raster::buffer(fence.sp, width=FB.dist)

#read in movement data
#ideally, the movement data should not have missing point. This trial file does have missing points.
movement.df.all <- read.csv("DataTable/pinedale.int2.raw.10ind.csv") 
movement.df.all$date <- as.POSIXct(strptime(as.character(movement.df.all$date),"%m/%d/%y %H:%M")) #change the format based on the data
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

################################################################################
# calculating average straightness for each moving window size between b and p for each individual -------
# return an animal table with animal ID, window size, year-round mean and sd straightness. Can be used to plot mean and confidence level
animal.stn.df <- data.frame(Location.ID = integer(), window.size = numeric(), Date = character(), Straightness = numeric())
for (i in unique(movement.df.all$Location.ID)) {
  movement.df.i <- movement.df.all[movement.df.all$Location.ID == i,]
  # only calculate straightness with time period that equals to the one that could be consider as trace or back-n-forth
  # e.g. interval = 2, b = 2, p = 24. Calculating moving window with size 3 - 24 (including 24)
  for (ii in (b+1):min(p, nrow(movement.df.i))) { # ii is the window size 
    straightness.ii <- vector()
    date.ii <- character()
    for (iii in seq(1, (nrow(movement.df.i)-ii), by = 10)) {  # can change "by" for different sampling rate to calculate strightness
      mov.seg.iii <- movement.df.i[iii:(iii+ii),]
      date.iii <- mov.seg.iii$date # mark the starting point of the calculated straightness 
      straightness.iii <- strtns(mov.seg.iii)
      straightness.ii <- c(straightness.ii, straightness.iii)
      date.ii <- c(date.ii, date.iii)
    }
    n <- length(straightness.ii)
    rows.i <- data.frame(Location.ID = rep(i, n), window.size = rep(ii, n), Date = date.ii, Straightness = straightness.ii)
    if (sum(is.na(row.i$Straightness)) != 0) {
      print(paste("i =", i, " ii =", ii, " iii =", iii))
      break
    }
    animal.stn.df <- rbind(animal.stn.df, rows.i)
  }
}
#write.csv(animal.stn.df, "/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/FenceBehavior/DataTable/straightness_roll_10ind.csv")

################################################################################
# classification step 1: classify everything but back-n-forth and trace --------

pb <- txtProgressBar(max=100, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)
# set an empty dataframe
event.df <- data.frame(AnimalID = character(), burstID = numeric(), easting = numeric(), northing = numeric(), duration = numeric(), straightness = numeric(), eventTYPE = character())

#start the loop
event.df = foreach (i = unique(movement.df.all$Location.ID), 
                    .combine=rbind,
                    .options.snow=opts,
                    .packages=c('raster', 'sp', 'rgdal', 'rgeos', 'adehabitatLT', 'sp', 'dplyr')) %dopar% { 
                      
  # --------------------- step 1: detect encounter event, build dataframe, burst ID = first timestamp -----------------------------
  movement.sp <- movement.sp.all[movement.sp.all$Location.ID == i, ]
  movement.df <- as.data.frame(movement.sp)
  #extract points that fall inside of the buffer 
  encounter.sp <- raster::intersect(movement.sp, fence.buffer)
  #Creat a data frame with encounter event marked as burst ID
  encounter.df <- as.data.frame(encounter.sp) 
  encounter.df <- encounter.df[which(!is.na(encounter.df$coords.x1)),]
  encounter.df$date <- as.POSIXct(strptime(as.character(encounter.df$date),"%Y-%m-%d %H:%M"))
  
  encounter.df$burst <- vector(length = nrow(encounter.df))
  for (ii in 2:nrow(encounter.df)){
    #if the time intervals between two points are within the set tolerance time, they belog to the same encounter event, marked as "m"
    if (difftime(encounter.df$date[ii], encounter.df$date[ii-1],units = "hours") <= (tolerance+1)*interval) {
      encounter.df$burst[ii-1] <- "m"  #mark the cell for the burst
      if (ii == nrow(encounter.df)) {
        encounter.df$burst[ii] <- "m"
      }
    }
    else {
      #all points before this point that are marked as "m" are in the same burst
      encounter.df$burst[ii-1] <- "m"
      burst.list <- encounter.df[which(encounter.df$burst == "m"),]
      encounter.df[which(encounter.df$burst == "m"),]$burst <- format(burst.list$date[1], "%y%m%d%H") #name each burst using the starting time stamp
      if (ii == nrow(encounter.df)) {
        encounter.df$burst[ii] <- "m"
      }
    }
    if (ii == nrow(encounter.df)) {
      burst.list <- encounter.df[which(encounter.df$burst == "m"),]
      if (nrow(burst.list) > 0) {
        encounter.df[which(encounter.df$burst == "m"),]$burst <- format(burst.list$date[1], "%y%m%d%H")
      }
    }
  }

  # clean the data frame
  encounter.df <- encounter.df[which(!is.na(encounter.df$burst)),]  #all points that are in buffer in one dataframe
  
  #------------------------ step 2: identify bouncing and cross, calculating duration and straightess  -------------------------------------
  # make a dataframe for this animal for layer be joined to the event.df later
  l <- length(unique(encounter.df$burst))
  event.df.i <- data.frame(AnimalID = character(l),  burstID = numeric(l), easting = numeric(l), northing = numeric(l), 
                           duration = numeric(l), straightness = numeric(l), eventTYPE = character(l), stringsAsFactors=FALSE)
  event.df.i$burstID <- unique(encounter.df$burst)
  event.df.i$AnimalID <- rep(i, l)
  
  # animal ID, in this dataframe is named as Location ID. 
  # Log ("Processing Animal ID %d, at %d. Total encounter events: %d", i, Sys.time(), l)
  
  for (ii in unique(encounter.df$burst)) {
    burst.i <- encounter.df[encounter.df$burst == ii, ]
    start.time <- burst.i[1,]$date
    end.time <- burst.i[nrow(burst.i),]$date
    event.df.i[which(event.df.i$burst==ii),]$duration <- difftime (end.time, start.time, units = "hours")
    event.df.i[which(event.df.i$burst==ii),]$easting <- burst.i$coords.x1[1]
    event.df.i[which(event.df.i$burst==ii),]$northing <- burst.i$coords.x2[1]
    
    #first for short encounter
    if (difftime (end.time, start.time, units = "hours") <= b*interval) {  #no more than b*interval H, only spend small amount of time in this burst
      pt.first <- burst.i[1,] #first point in the burst
      mov.seg.i <- movement.segment.b(pt.first$ptsID) #extract movement segment between the first point pt1 and pt+ b (if they cross in bth steps, not a barrier response)
      #here is another measurement that prefer smaller time interval movement data. Big interval data between point distance can misrepresent real trajectory
      int.num <- length(gIntersection(mov.seg.i, fence.sp)) 
      ############# Need to acknowledge possibility for mistakes here since the links between points are not the movement route. 
      if (int.num == 0) {
        event.df.i[which(event.df.i$burst==ii),]$eventTYPE <- "Bounce"   
      }
      else {
        event.df.i[which(event.df.i$burst==ii),]$eventTYPE <- "Crossed" #meaning not a barrier encounter
      }
    }
    #next for longer encounter
    else { #If the point b timesteps later out is still in the same burst/encountering event (the animal does have some extensive interactions with the barrier
      if (difftime(end.time, start.time, units = "hours") > p*interval) {
        event.df.i[which(event.df.i$burst==ii),]$eventTYPE <- "Trapped" #could also be a home ranging behavior. Can be differenciate by time
      }
      else {
        # all points in the burst into one movement trajectory
        mov.seg.i <- encounter.df[which(encounter.df$burst==ii),]  
        # calculating straightness of the encounter event 
        straightness <- strtns(mov.seg.i) 
        event.df.i[which(event.df.i$burst==ii),]$straightness <- straightness
        event.df.i[which(event.df.i$burst==ii),]$eventTYPE <- "TBD"
        # out put a window with raw straightness value. Classify later. 
      }
    }
  }
  event.df.i #this will be attached to the event.df in the "for each" loop
}

#write.csv(event.df, file = "pinedale_int3_Types_Bf500_allwithGeo.csv")
close(pb)
#stop cluster
stopCluster(cl)

################################################################################
# classification step 2: classify back-n-forth and trace -----------------------
# based on comparing average straightness around the encounter event -----------




# # ---- for visual interpretation ----- 
# i <- unique(encounter.df$burst)[5]
# burst.i <- encounter.df[encounter.df$burst == i,]
# pt.first <- burst.i[1,] #first poiny in the burst
# pt.last <- burst.i[nrow(burst.i),] #last point in the burst
# nrow(burst.i)
# mov.seg.i <- encounter.df[which(encounter.df$burst==i),]  
# pts <- cbind(mov.seg.i$coords.x1, mov.seg.i$coords.x2)
# i.traj <- as.ltraj(xy =  pts, date = mov.seg.i$date, id = mov.seg.i$ID, burst = mov.seg.i$burst)
# traj.dist <- sqrt(
#   (i.traj[[1]]$x[1]-i.traj[[1]]$x[nrow(i.traj[[1]])])*(i.traj[[1]]$x[1]-i.traj[[1]]$x[nrow(i.traj[[1]])]) +
#     (i.traj[[1]]$y[1]-i.traj[[1]]$y[nrow(i.traj[[1]])])*(i.traj[[1]]$y[1]-i.traj[[1]]$y[nrow(i.traj[[1]])]) 
#   )
# traj.lgth <- sum(i.traj[[1]]$dist, na.rm = TRUE)
# straightness <- traj.dist/traj.lgth
# straightness
# plot(i.traj, xlim=c(min(pts[,1]-200), max(pts[,1]+200)), ylim=c(min(pts[,2]-200), max(pts[,2]+200)))
# plot(fence.sp, add = T)
# #plot(fence.buffer, col = "gray", add= T)


# # -----add lat long-
# # xy <- cbind(event.df$easting, event.df$northing)
# # event <- SpatialPointsDataFrame (coords = xy, data = event.df, proj4string = CRS(target.crs))
# # proj4string(event)
# # event.ll <- event.df
# coordinates(event.df) <- ~easting + northing
# proj4string(event.df) <- CRS("+proj=utm +zone=12")
# event.ll <- spTransform(event.df, CRS("+proj=longlat")) 
# event.ll <- cbind(event.ll, coordinates(event.ll))
# event.ll$time <- as.POSIXct(strptime(as.character(event.ll$burstID),"%y%m%d%H"))  
# write.csv(event.ll, file = "pinedale_int3_Types_Bf500_allwitLatlong.csv")
