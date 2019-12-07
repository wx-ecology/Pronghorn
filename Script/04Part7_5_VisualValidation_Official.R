################ Barrier Behavior Analysis (BaBA): visual interpretation(validation)  ###########
# time: 11/30/2019
# description: this is to visualize encounter event so that user can visually identify behaviral types based on BaBA catagories

## input ##
# final classification table 
# encounter event table 

#############################
######### Set-up ###########
#############################
#############################
setwd("C:\\Users\\wenjing.xu\\Google Drive\\RESEARCH\\Pronghorn\\Analysis\\FenceBehavior_Official")


event.df <- read.csv("I2_MULE_FB109_B4_P36_FinalCls.csv")
encounter.df <- read.csv("I2_MULE_FB109_B4_P36_EncounterEvents.csv")
encounter.df$date <- as.POSIXct(strptime(as.character(encounter.df$date),"%Y-%m-%d %H:%M"))
movement.df.all <- movement.df.all <- read.csv("Int2_MULE_Raw_All.csv") 
movement.df.all$date <- as.POSIXct(strptime(as.character(movement.df.all$date),"%m/%d/%Y %H:%M")) #change the format based on the data
movement.df.all <- movement.df.all <- movement.df.all[(!is.na(movement.df.all$date))&(!is.na(movement.df.all$Easting)),]


# prepare spatial dataframe -----------------------------------
#read in fence data
fence.filename <- 'Fence_convex_FINAL'
fence.sp <- readOGR(".", fence.filename)
#fence.sp <- spTransform(fence.sp,target.crs)
fence.buffer <- raster::buffer(fence.sp, width=FB.dist)



# ---- random samples for visualizations
# --- method 1: randomly select encounter events to visually classify ------
n <- nrow(event.df)
m <- round (n*0.1)
set.seed(7)
samples <- sort(sample(1:n, m))

# export sample table 
# sampletable <- event.df %>% filter(X %in% samples)
# write.csv(sampletable, "Mule_Validation.csv")

for (i in samples) {  # i is event ID. 
  event.i <- event.df[i,]
  Type <- event.i$eventTYPE
  AnimalID.i <- event.i$AnimalID
  burstID.i <- event.i$burstID
  mov.seg.i <- encounter.df[(encounter.df$burst == burstID.i)
                            & (encounter.df$Location.ID == AnimalID.i),]
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
  plot(fence.buffer, add = T)
  plot(fence.sp, lwd=2, add = T)
  
  readline(prompt= paste0("No.", which(samples ==i), " sample, event ID is ", i, 
                          ", sample type is ", Type, ", ", m-(which(samples ==i)), " to go." ))
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
  plot(fence.buffer, add = T)
  plot(fence.sp, lwd=2, add = T)
  plot(i.traj, add = T)
}

ploti(218)
