################ What do animals do in face of barriers ######################################
# Time of first creation: 07052019
## Input: Fence shp, movement points (with the same time intervals since the behavior type assumption 
# is related to how much time is represented between two movement points)
## output: a table with animal ID, encounter event ID (indicating starting time YYMMDDHH), # of movement points, straightness index (if calculated), classified type

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

setwd("C:\\Users\\wenjing.xu\\Google Drive\\RESEARCH\\Pronghorn\\Analysis\\FenceBehavior")
#setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/FenceBehavior")

#############################
#########Parameters##########
#############################
target.crs <- "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
# ----- costamized parameter 1 --------------------------
interval <- 3 #define time interval (in hours) of the movement data
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

# prepare dataframe -----------------------------------
#read in fence data
fence.filename <- 'Fence062219'
fence.sp <- readOGR(".", fence.filename)
fence.sp <- spTransform(fence.sp,target.crs)
fence.buffer <- raster::buffer(fence.sp, width=FB.dist)

#read in movement data
#ideally, the movement data should not have missing point. If there is, maybe preprocess the movement data and interpolate middle points 
movement.df.all <- read.csv("DataTable\\pinedale.int3.raw.csv") 
movement.df.all$date <- as.POSIXct(strptime(as.character(movement.df.all$date),"%m/%d/%Y %H:%M"))
# add point ID by individual
movement.df.all$ptsID <- numeric(nrow(movement.df.all))
for (i in unique(movement.df.all$Location.ID)) {
  mov.seg.i <- movement.df.all[movement.df.all$Location.ID==i,]
  movement.df.all[movement.df.all$Location.ID==i,]$ptsID <- seq(nrow(mov.seg.i))
}
xy <- cbind(movement.df.all$Easting, movement.df.all$Northing)
#movement.traj <- as.ltraj(xy = xy, date = movement.df.all$date, id = movement.df.all$Location.ID, proj4string = CRS(target.crs)) 
movement.sp.all <- SpatialPointsDataFrame (coords = xy, data = movement.df.all, proj4string = CRS(target.crs))


# classification --------------------------------------
event.df <- data.frame(AnimalID = character(), burstID = numeric(), duration = numeric(), straightness = numeric(), eventTYPE = character())
for (i in unique(movement.df.all$Location.ID)) { #animal ID, in this dataframe is named as Location ID. 
  print(paste0("AnimalID",i))
  # --------------------- step 1: creat encounter event dataframe, burst ID = first timestamp -----------------------------
  movement.sp <- movement.sp.all[movement.sp.all$Location.ID == i, ]
  movement.df <- as.data.frame(movement.sp)
  #extract points that fall inside of the buffer 
  encounter.sp <- raster::intersect(movement.sp, fence.buffer)
  #Creat a data frame with encounter event marked as burst ID
  encounter.df <- as.data.frame(encounter.sp) 
  encounter.df <- encounter.df[which(!is.na(encounter.df$coords.x1)),]
  encounter.df$date <- as.POSIXct(strptime(as.character(encounter.df$date),"%Y-%m-%d %H:%M"))
  
  encounter.df$burst <- NA
  for (ii in 2:nrow(encounter.df)){
    if (difftime(encounter.df$date[ii],encounter.df$date[ii-1],units = "hours") <= (tolerance+1)*interval) {
      encounter.df$burst[ii-1] <- "m"  #mark the cell for the burst
    }
    else {
      encounter.df$burst[ii-1] <- "m"
      burst.list <- encounter.df[which(encounter.df$burst == "m"),]
      encounter.df[which(encounter.df$burst == "m"),]$burst <- format(burst.list$date[1], "%y%m%d%H") #name each burst using the starting time stamp
    }
  }
  #the last set of burst would be omited in the loop. Fix the issue.
  #which(encounter.df$burst == "m") #the row numbers should be in a continuous series (including only one)
  burst.list <- encounter.df[which(encounter.df$burst == "m"),]
  if (nrow(burst.list) > 0) {
    encounter.df[which(encounter.df$burst == "m"),]$burst <- format(burst.list$date[1], "%y%m%d%H")
  }
  
  # clean the data frame
  encounter.df <- encounter.df[which(!is.na(encounter.df$burst)),]  #all points that are in buffer in one dataframe
  
  # make the dataframe for this animal for layer be joined to the event.df
  l <- length(unique(encounter.df$burst))
  event.df.i <- data.frame(AnimalID = character(l), burstID = numeric(l), duration = numeric(l), straightness = numeric(l), eventTYPE = character(l), stringsAsFactors=FALSE)
  event.df.i$burstID <- unique(encounter.df$burst)
  event.df.i$AnimalID <- rep(i, l)
  print(paste0("total burst",l))
  
  # ------------------------ step 2: classification ----------------------------------------------------------------------
  

    for (ii in unique(encounter.df$burst)) {
      print(paste0(i,"-",ii))
    burst.i <- encounter.df[encounter.df$burst == ii, ]
    start.time <- burst.i[1,]$date
    end.time <- burst.i[nrow(burst.i),]$date
    event.df.i[which(event.df.i$burst==ii),]$duration <- difftime (end.time, start.time, units = "hours")
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
        event.df.i[which(event.df.i$burst==ii),]$eventTYPE <- "None" #meaning not a barrier encounter
      }
    }
    #next for longer encounter
    else { #If the point b timesteps later out is still in the same burst/encountering event (the animal does have some extensive interactions with the barrier
      mov.seg.i <- encounter.df[which(encounter.df$burst==ii),]  # all points in the burst
      pts <- cbind(mov.seg.i$coords.x1, mov.seg.i$coords.x2)
      pts.sp <- SpatialPointsDataFrame(pts, mov.seg.i, proj4string = CRS(target.crs))
      i.traj <- as.ltraj(xy =  pts, date = mov.seg.i$date, id = mov.seg.i$Location.ID, burst = mov.seg.i$burst)
      #moving distance from first pt to last pt in the burst
      traj.dist <- sqrt(
        (i.traj[[1]]$x[1]-i.traj[[1]]$x[nrow(i.traj[[1]])])*(i.traj[[1]]$x[1]-i.traj[[1]]$x[nrow(i.traj[[1]])]) +
          (i.traj[[1]]$y[1]-i.traj[[1]]$y[nrow(i.traj[[1]])])*(i.traj[[1]]$y[1]-i.traj[[1]]$y[nrow(i.traj[[1]])]) 
      )
      #sum of all step lengths
      traj.lgth <- sum(i.traj[[1]]$dist, na.rm = TRUE)
      #straightness ranges from 0 to 1. More close to 0 more sinuous it is.
      straightness <- traj.dist/traj.lgth
      event.df.i[which(event.df.i$burst==ii),]$straightness <- straightness
      if (straightness > 0.7) { #this cutoff point needs to be discussed
        event.df.i[which(event.df.i$burst==ii),]$eventTYPE <- "Along"
      }
      else if(straightness < 0.4) { #this cutoff point needs to be discussed
        if (difftime(end.time, start.time, units = "hours") <= p*interval)  { #no more than p points in the burst
          event.df.i[which(event.df.i$burst==ii),]$eventTYPE <- "Back-n-forth"
        }
        else {
          event.df.i[which(event.df.i$burst==ii),]$eventTYPE <- "Trapped" #could also be a home ranging behavior. Can be differenciate by time
        }
      }
      else {
        event.df.i[which(event.df.i$burst==ii),]$eventTYPE <- "TBD"
      }
    }
  }
  #------- step 3:dataframe summarizing encounting events
  event.df <- rbind(event.df, event.df.i)
    }

#write.csv(event.df, file = "pinedale_int3_Types_Bf500.csv")



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
