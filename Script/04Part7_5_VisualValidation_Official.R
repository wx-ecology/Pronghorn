################ Barrier Behavior Analysis (BaBA): visual interpretation(validation)  ###########
# time: 11/30/2019
# description: this is to visualize encounter event so that user can visually identify behaviral types based on BaBA catagories

## input ##
# final classification table 
# encounter event table 

#############################
######### Set-up ###########
#############################
event.df <- read.csv("I2_All_FB180_B4_P36_FinalClassification.csv")
encounter.df <- read.csv("I2_All_FB180_B4_P36_EncounterEvents.csv")

event.df$burstID <- as.POSIXct(strftime(event.df$burstID, "%Y-%m-%d %H:%M"))

# ---- random samples for visualizations
# --- method 1: randomly select encounter events to visually classify ------
n <- length(unique(event.df$burst))
m <- round (n*0.1)
set.seed(7)
samples <- sort(sample(1:n, m))

for (i in samples) {
  mov.seg.i <- encounter.df[(encounter.df$burst == unique(encounter.df$burst)[i])
                            & (encounter.df$Location.ID == event.df$AnimalID[i]),]
  X <- event.df[event.df$AnimalID == mov.seg.i$Location.ID[1],]
  Type <- X[X$burstID == mov.seg.i$burst[1],]$eventTYPE
  if ((Type == "Bounce") | (Type == "Quick Cross")) {
    mov.seg.large.i <- movement.df.all[(mov.seg.i[1,1]-1):(mov.seg.i[nrow(mov.seg.i),1]+1) ,  ]  #the trajectory with one point before and one point after
    pts <- cbind(mov.seg.large.i$Easting, mov.seg.large.i$Northing)
    i.traj <- as.ltraj(xy =  pts, date = mov.seg.large.i$date, id = mov.seg.large.i$Location.ID)
  }
  else { 
    pts <- cbind(mov.seg.i$Easting, mov.seg.i$Northing)
    i.traj <- as.ltraj(xy =  pts, date = mov.seg.i$date, id = mov.seg.i$Location.ID)
  }
  
  plot(i.traj, xlim=c(min(pts[,1]-500), max(pts[,1]+500)), ylim=c(min(pts[,2]-500), max(pts[,2]+500)))
  #plot(fence.buffer, col = rgb(0.36,0.57,0.28), xlim=c(min(pts.large[,1]-200), max(pts.large[,1]+200)), ylim=c(min(pts.large[,2]-200), max(pts.large[,2]+200)), add = T)
  plot(fence.sp, lwd=2, add = T)
  readline(prompt= paste0("No.", which(samples ==i), " sample, sample is ", X[X$burstID == mov.seg.i$burst[1],]$X, 
                          ", sample type is ", X[X$burstID == mov.seg.i$burst[1],]$eventTYPE, ", ", m-(which(samples ==i)), " to go." ))
}

# --- method 2: Identify specific event by row # to visually classify ------
ploti <- function (i) {
  burst <- event.df[i,]$burstID
  mov.seg.i <- encounter.df[(encounter.df$burst == burst)
                            & (encounter.df$Location.ID == event.df[i,]$AnimalID),]
  Type <- event.df[i,]$eventTYPE
  if ((Type == "Bounce") | (Type == "Quick Cross")) {
    mov.seg.large.i <- movement.df.all[(mov.seg.i[1,1]-1):(mov.seg.i[nrow(mov.seg.i),1]+1) ,  ]  #the trajectory with one point before and one point after
    pts <- cbind(mov.seg.large.i$Easting, mov.seg.large.i$Northing)
    i.traj <- as.ltraj(xy =  pts, date = mov.seg.large.i$date, id = mov.seg.large.i$Location.ID)
  }
  if  ((Type != "Bounce") & (Type != "Quick Cross")) { 
    pts <- cbind(mov.seg.i$Easting, mov.seg.i$Northing)
    i.traj <- as.ltraj(xy =  pts, date = mov.seg.i$date, id = mov.seg.i$Location.ID)
  }
  
  plot(i.traj, xlim=c(min(pts[,1]-500), max(pts[,1]+500)), ylim=c(min(pts[,2]-500), max(pts[,2]+500)))
  #plot(fence.buffer, col = rgb(0.36,0.57,0.28), xlim=c(min(pts.large[,1]-200), max(pts.large[,1]+200)), ylim=c(min(pts.large[,2]-200), max(pts.large[,2]+200)), add = T)
  plot(fence.sp, lwd=2, add = T)
}

ploti(218)