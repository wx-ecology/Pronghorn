################ Barrier Behavior Analysis (BaBA): visual interpretation(validation)  ###########
# time: 11/30/2019
# description: this is to visualize encounter event so that user can visually identify behaviral types based on BaBA catagories

## input ##
# final classification table 
# encounter event table 


#############################
######### Set-up ###########
#############################

# ----- libraries -------
library(dplyr)
# spatial analysis
library(rgdal)
library(rgeos)
library(sp)
library(raster)
#trajectory analysis
library(adehabitatLT)

setwd("C:\\Users\\wenjing.xu\\Google Drive\\RESEARCH\\Pronghorn\\Analysis\\FenceBehavior_Official")

movement.df.all <- movement.df.all <- read.csv("Int2_PRON_Raw_Final.csv") 
movement.df.all$date <- as.POSIXct(strptime(as.character(movement.df.all$date),"%m/%d/%Y %H:%M")) #change the format based on the data
movement.df.all <- movement.df.all <- movement.df.all[(!is.na(movement.df.all$date))&(!is.na(movement.df.all$Easting)),]


FB.dist <- 90
event.df <- read.csv(paste0("I2_PRON_FB", FB.dist, "_B4_P36_FinalCls.csv"))
event.df$burstID <- as.character(event.df$burstID)
encounter.df <- read.csv(paste0("I2_PRON_FB", FB.dist, "_B4_P36_EncounterEvents.csv"))
encounter.df$burst <- as.character(encounter.df$burst)
encounter.df$date <- as.POSIXct(strptime(as.character(encounter.df$date),"%Y-%m-%d %H:%M"))

# check the result table 
summary(event.df$eventTYPE)


# prepare spatial dataframe -----------------------------------
#read in fence data
fence.filename <- 'Fence_convex_FINAL'
fence.sp <- readOGR(".", fence.filename)
#fence.sp <- spTransform(fence.sp,target.crs)
fence.buffer <- raster::buffer(fence.sp, width=FB.dist)


# ---- random samples for visualizations
# --- method 1: randomly select encounter events to visually classify ------
event.df %>% filter(eventTYPE == "unknown")
n <- nrow(event.df)
m <- round (n*0.5)
set.seed(7)
samples <- sort(sample(1:n, m))

sample.table <- event.df[samples,]
#write.csv(sample.table, "PRON_fb100_visual_trace_samples.csv")

for (i in samples) {  # i is event ID. 
  event.i <- event.df[i,]
  Type <- event.i$eventTYPE
  AnimalID.i <- event.i$AnimalID
  burstID.i <- event.i$burstID
  mov.seg.i <- encounter.df[(encounter.df$burst == burstID.i)
                            & (encounter.df$Location.ID == AnimalID.i),]
  if ((Type == "Bounce") | (Type == "Quick Cross")) {
    movement.df.i <- movement.df.all %>% filter(Location.ID == AnimalID.i)
    mov.seg.large.i <- movement.df.i[(mov.seg.i$ptsID[1]-1):(mov.seg.i$ptsID[nrow(mov.seg.i)]+1) ,  ]  #the trajectory with one point before and one point after
    pts <- cbind(mov.seg.large.i$Easting, mov.seg.large.i$Northing)
    i.traj <- as.ltraj(xy =  pts, date = mov.seg.large.i$date, id = mov.seg.large.i$Location.ID)
  }
  else { 
    pts <- cbind(mov.seg.i$Easting, mov.seg.i$Northing)
    i.traj <- as.ltraj(xy =  pts, date = mov.seg.i$date, id = mov.seg.i$Location.ID)
  }
  
  plot(i.traj, xlim=c(min(pts[,1]-500), max(pts[,1]+500)), ylim=c(min(pts[,2]-500), max(pts[,2]+500)))
  #plot(fence.buffer, col = rgb(0.36,0.57,0.28), xlim=c(min(pts.large[,1]-200), max(pts.large[,1]+200)), ylim=c(min(pts.large[,2]-200), max(pts.large[,2]+200)), add = T)
  plot(fence.buffer, add = T)
  plot(fence.sp, lwd=2, add = T)
  title(main = paste0(Type))
  
  readline(prompt= paste0("No.", which(samples ==i), " sample, event ID is ", i, 
                          ", sample type is ", Type, ", ", m-(which(samples ==i)), " to go." ))
}

# --- method 2: Identify specific event by row # to visually classify ------
ploti <- function (i) {
  event.i <- event.df[i,]
  Type <- event.i$eventTYPE
  AnimalID.i <- event.i$AnimalID
  burstID.i <- event.i$burstID
  mov.seg.i <- encounter.df[(encounter.df$burst == burstID.i)
                            & (encounter.df$Location.ID == AnimalID.i),]
  if ((Type == "Bounce") | (Type == "Quick Cross")) {
    mov.seg.large.i <- movement.df.i[(mov.seg.i$ptsID[1]-1):(mov.seg.i$ptsID[nrow(mov.seg.i)]+1) ,  ]   #the trajectory with one point before and one point after
    pts <- cbind(mov.seg.large.i$Easting, mov.seg.large.i$Northing)
    i.traj <- as.ltraj(xy =  pts, date = mov.seg.large.i$date, id = mov.seg.large.i$Location.ID)
  }
  if  ((Type != "Bounce") & (Type != "Quick Cross")) { 
    pts <- cbind(mov.seg.i$Easting, mov.seg.i$Northing)
    i.traj <- as.ltraj(xy =  pts, date = mov.seg.i$date, id = mov.seg.i$Location.ID)
  }
  
  plot(i.traj, xlim=c(min(pts[,1]-500), max(pts[,1]+500)), ylim=c(min(pts[,2]-500), max(pts[,2]+500)))
  #plot(fence.buffer, col = rgb(0.36,0.57,0.28), xlim=c(min(pts.large[,1]-200), max(pts.large[,1]+200)), ylim=c(min(pts.large[,2]-200), max(pts.large[,2]+200)), add = T)
  plot(fence.buffer, add = T)
  plot(fence.sp, lwd=2, add = T)
  title(main = paste0(Type))
}

ploti(117)
