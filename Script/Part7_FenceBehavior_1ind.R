# What do animals do in face of barriers 
# Time of first creation: 07012019
# Input: Fence shp, movement points 
# output: a table with animal ID, encounter event ID (indicating starting time YYMMDDHH), # of movement points, classified type

# current types are: bounce, along, back and forth (lingering would be a special case of this kind), 
# trapped (could also be a home range behavior), none (not a barrier response), and TBD (hard to classify)

#############################################################################################
# ----- set up -------
library(rgdal)
library(rgeos)
library(adehabitatLT)
library(sp)
library(circular)
library(dplyr)
library(raster)

#setwd("C:\\Users\\wenjing.xu\\Google Drive\\RESEARCH\\Pronghorn\\Analysis\\FenceBehavior\\IndividualTrial")
setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/FenceBehavior/IndividualTrial")

#read in fence data
fence.filename <- 'Fence062219'
fence.sp <- readOGR(".", fence.filename)


#read in movement data
movement.filename <- 'PAPO_79'
movement.sp <- readOGR(".", movement.filename)
movement.sp$ptsID <- seq(1:nrow(movement.sp))  #for each individual, pt ID is a continuous numeric number 
movement.df <- as.data.frame(movement.sp)
movement.df$date <- as.POSIXct(strptime(as.character(movement.df$date),"%Y-%m-%d %H:%M")) #all points in one dataframe

# ----- costamized parameter 1 --------------------------
interval <- 3 #define time interval of the movement data
ID <- "PAPO_79"
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
b <- 2
# minimum # of points in the burst that you'd call it a trap
p <- 24

#############################################################################################
# ---- step 1: creat encounter event dataframe, burst ID = first timestamp ---------
#project two dataset (UTM zone 12N)
target.crs <- proj4string(movement.sp)
fence.sp <- spTransform(fence.sp,target.crs)

# Fence encounter burst ------
fence.buffer <- raster::buffer(fence.sp, width=FB.dist)

# #visual check
# plot(movement.sp, main = paste("Pronghorn:", movement.filename), pch = 20)
# plot(fence.sp, add = T)
# plot(fence.buffer, add = T)

#extract points that fall inside of the buffer 
encounter.sp <- raster::intersect(movement.sp, fence.buffer)
#Creat a data frame with encounter event marked as burst ID
encounter.df <- as.data.frame(encounter.sp) 
encounter.df <- encounter.df[which(!is.na(encounter.df$coords.x1)),]
encounter.df$date <- as.POSIXct(strptime(as.character(encounter.df$date),"%Y-%m-%d %H:%M"))

encounter.df$burst <- NA
for (i in 2:nrow(encounter.df)){
  if (difftime(encounter.df$date[i],encounter.df$date[i-1],units = "hours") <= (tolerance+1)*interval) {
    encounter.df$burst[i-1] <- "m"  #mark the cell for the burst
  }
  else {
    encounter.df$burst[i-1] <- "m"
    burst.list <- encounter.df[which(encounter.df$burst == "m"),]
    encounter.df[which(encounter.df$burst == "m"),]$burst <- format(burst.list$date[1], "%y%m%d%H") #name each burst using the starting time stamp
  }
}

#the last set of burst would be omited in the loop. Fix the issue.
#which(encounter.df$burst == "m") #the row numbers should be in a continuous series (including only one)
burst.list <- encounter.df[which(encounter.df$burst == "m"),]
encounter.df[which(encounter.df$burst == "m"),]$burst <- format(burst.list$date[1], "%y%m%d%H")

# clean the data frame
encounter.df <- encounter.df[which(!is.na(encounter.df$burst)),]  #all points that are in buffer in one dataframe
encounter.df$ID <- ID #Add ID to the dataframe


#############################################################################################
# ---- step 2: classify encounter events into traped, searching, there-and-back, moving along -----

#############################
#########Functions###########
#############################
movement.segment.b <- function(m) { # extract movement segment between time m and predetermined time lap b. 
  n <- m + b
  segments <- movement.df[which(movement.df$ptsID >= m & movement.df$ptsID <=n),]
  seg.line <- Lines(Line(cbind(segments$coords.x1, segments$coords.x2)), ID = segments$date[1])
  segments.sp <- SpatialLines(list(seg.line), proj4string = CRS(target.crs))
  return(segments.sp)
}

#############################
###### Classification #######
#############################
encounter.df$Type <- character(nrow(encounter.df))
encounter.df$Straightness <- numeric(nrow(encounter.df))
for (i in unique(encounter.df$burst)) {
  burst.i <- encounter.df[encounter.df$burst == i, ]
  #first for short encounter
  if (nrow(burst.i) <= b) {  #no more than b points in the burst
    pt.first <- burst.i[1,] #first point in the burst
    mov.seg.i <- movement.segment.b(pt.first$ptsID) #extract movement segment between the first point pt1 and pt+ b (if they cross in bth steps, not a barrier response)
    int.num <- length(gIntersection(mov.seg.i, fence.sp))
    ############# Need to acknowledge possibility for mistakes here since the links between points are not the movement route. 
    if (int.num == 0) {
      encounter.df[which(encounter.df$burst==i),]$Type <- "Bounce"   
    }
    else {
      encounter.df[which(encounter.df$burst==i),]$Type <- "None" #meaning not a barrier encounter
    }
  }
  #next for longer encounter
  else { #If the point b timesteps later out is still in the same burst/encountering event (the animal does have some extensive interactions with the barrier
    mov.seg.i <- encounter.df[which(encounter.df$burst==i),]  # all points in the burst
    pts <- cbind(mov.seg.i$coords.x1, mov.seg.i$coords.x2)
    pts.sp <- SpatialPointsDataFrame(pts, mov.seg.i, proj4string = CRS(target.crs))
    i.traj <- as.ltraj(xy =  pts, date = mov.seg.i$date, id = mov.seg.i$ID, burst = mov.seg.i$burst)
    #moving distance from first pt to last pt in the burst
    traj.dist <- sqrt(
      (i.traj[[1]]$x[1]-i.traj[[1]]$x[nrow(i.traj[[1]])])*(i.traj[[1]]$x[1]-i.traj[[1]]$x[nrow(i.traj[[1]])]) +
        (i.traj[[1]]$y[1]-i.traj[[1]]$y[nrow(i.traj[[1]])])*(i.traj[[1]]$y[1]-i.traj[[1]]$y[nrow(i.traj[[1]])]) 
    )
    #sum of all step lengths
    traj.lgth <- sum(i.traj[[1]]$dist, na.rm = TRUE)
    #straightness ranges from 0 to 1. More close to 0 more sinuous it is.
    straightness <- traj.dist/traj.lgth
    encounter.df[which(encounter.df$burst==i),]$Straightness <- straightness
    if (straightness > 0.7) { #this cutoff point needs to be discussed
      encounter.df[which(encounter.df$burst==i),]$Type <- "Along"
    }
    else if(straightness < 0.4) { #this cutoff point needs to be discussed
      if (nrow(burst.i) <= p)  { #no more than p points in the burst
        encounter.df[which(encounter.df$burst==i),]$Type <- "Back-n-forth"
      }
      else {
        encounter.df[which(encounter.df$burst==i),]$Type <- "Trapped" #could also be a home ranging behavior. Can be differenciate by time
      }
    }
    else {
      encounter.df[which(encounter.df$burst==i),]$Type <- "TBD"
    }
  }
}

#create a dataform summarizing encounting events
event.df <- encounter.df %>% group_by(burst) %>% summarise(first(Type))
event.df <- as.data.frame(event.df)
colnames(event.df) <- c("eventID", "eventTYPE")
event.df$npts <- (encounter.df %>% group_by(burst) %>% summarise(n()))[,2]
event.df$AnimalID <- rep(encounter.df$ID[1], nrow(event.df))


# ---- for visual interpretation ----- 
i <- unique(encounter.df$burst)[35]
burst.i <- encounter.df[encounter.df$burst == i,]
pt.first <- burst.i[1,] #first poiny in the burst
pt.last <- burst.i[nrow(burst.i),] #last point in the burst
nrow(burst.i)
mov.seg.i <- encounter.df[which(encounter.df$burst==i),]  
pts <- cbind(mov.seg.i$coords.x1, mov.seg.i$coords.x2)
i.traj <- as.ltraj(xy =  pts, date = mov.seg.i$date, id = mov.seg.i$ID, burst = mov.seg.i$burst)
traj.dist <- sqrt(
  (i.traj[[1]]$x[1]-i.traj[[1]]$x[nrow(i.traj[[1]])])*(i.traj[[1]]$x[1]-i.traj[[1]]$x[nrow(i.traj[[1]])]) +
    (i.traj[[1]]$y[1]-i.traj[[1]]$y[nrow(i.traj[[1]])])*(i.traj[[1]]$y[1]-i.traj[[1]]$y[nrow(i.traj[[1]])]) 
  )
traj.lgth <- sum(i.traj[[1]]$dist, na.rm = TRUE)
straightness <- traj.dist/traj.lgth
straightness
plot(i.traj, xlim=c(min(pts[,1]-200), max(pts[,1]+200)), ylim=c(min(pts[,2]-200), max(pts[,2]+200)))
plot(fence.sp, add = T)
#plot(fence.buffer, col = "gray", add= T)
