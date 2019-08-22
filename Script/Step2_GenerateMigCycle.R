##### STEP 2: Prepare the data to be only for migrations by year
# including cut locations to fit calendar year, and add info (whether they are enought to cover migration cycles) on the migration info
# Created: Jan 2019

# Input: FINAL_PAPOanimalTableWX.csv & FINAL_JMHanimalTableWX.csv
# Output: PAPO.Mig.Cycle.csv & JMH.Mig.Cycle

# set up ----------------------------------------------
# some of the migration cycles do not have 365 day of data because of the differences in schedule drop off

setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis")
library(adehabitatLT)
# create location data that is exactly one year (PAPO) and create animal info sheet (insert NA) -------------

PAPO <- read.csv("data/FINAL_PAPOlocationTableWX.csv")
PAPO$time <- as.POSIXct(strptime(as.character(PAPO$time), "%Y-%m-%d %H:%M"))

PAPO.info <- read.csv("data/FINAL_PAPOanimalTableWX.csv")
PAPO.info$ef.mig <- as.character(PAPO.info$ef.mig)
PAPO.info$Location.ID <- as.factor(PAPO.info$Location.ID)

PAPO.Mig.Cycle <- data.frame()
for (i in levels(PAPO.info$Location.ID)) {
  animal <- PAPO[PAPO$Location.ID == i,]
  first.row.id <- which (animal$Month == 2) [1] #find the first row of Feb in the effective year. PAPO all started in Dec or Jan
  if (is.na(first.row.id)) {
    PAPO.info[which(PAPO.info$Location.ID == i),]$ef.mig <- NA
    next}
  first.row.time <- animal$time[first.row.id]
  last.row.time <- animal$time[nrow(animal)]
  last.supposed.time <- (animal[first.row.id, "time"] + 365*60*60*24)
  if (last.row.time < last.supposed.time) {
    last.month <- as.numeric(format(last.row.time, "%m"))
    if ((last.month < 4) & (as.numeric(format(last.row.time, "%Y")) ==  as.numeric(format(first.row.time, "%Y")))) {
      PAPO.info[which(PAPO.info$Location.ID == i),]$ef.mig <- NA
      next
    }
    else {
      animal <- animal[first.row.id:nrow(animal),]  #at least can cover one migration season, so use the data
      if ( (last.month >= 11 )  |  (last.month == 1) ) {
        PAPO.info[which(PAPO.info$Location.ID == i),]$ef.mig <- "Sufficient" #should be enough to cover both migration season
        PAPO.info[which(PAPO.info$Location.ID == i),]$ef.year <- as.numeric(format(first.row.time, "%Y"))
      }
      else {
        PAPO.info[which(PAPO.info$Location.ID == i),]$ef.mig <- "Spring Only"  #only spring, at least the onset, is covered (at least has data covering April)
        PAPO.info[which(PAPO.info$Location.ID == i),]$ef.year <- as.numeric(format(first.row.time, "%Y"))
      }
    }
  }
  else {
    last.row.id <- which(animal$time >= last.supposed.time) [1]
    animal <- animal[first.row.id:last.row.id,]
    PAPO.info[which(PAPO.info$Location.ID == i),]$ef.mig <- "Compelete Year"
    PAPO.info[which(PAPO.info$Location.ID == i),]$ef.year <- as.numeric(format(first.row.time, "%Y"))
  }
  PAPO.Mig.Cycle <- rbind(PAPO.Mig.Cycle, animal)
}

PAPO.Mig.Cycle$time <- as.POSIXct(strptime(as.character(PAPO.Mig.Cycle$time), "%Y-%m-%d %H:%M"))
# write.csv(PAPO.Mig.Cycle, "data/PAPO.Mig.Cycle.csv")
# write.csv(PAPO.info, "data/FINAL_PAPOanimalTableWX.csv")

# test whether it is good to generate traj.
#PAPO.traj <- as.ltraj(xy = PAPO.Mig.Cycle[, c("Easting", "Northing")], date = PAPO.Mig.Cycle$time, id = PAPO.Mig.Cycle$Location.ID)
 
# create location data that is exactly one year (JMH) -------------
JMH <- read.csv("data/FINAL_JMHlocationTableWX.csv")
JMH$time <- as.POSIXct(strptime(as.character(JMH$time), "%Y-%m-%d %H:%M"))

JMH.info <- read.csv("data/FINAL_JMHanimalTableWX.csv")
JMH.info$ef.mig <- as.character(JMH.info$ef.mig)
JMH.info$Location.ID <- as.factor(JMH.info$Location.ID)

JMH.Mig.Cycle <- data.frame()
for (i in levels(JMH.info$Location.ID)) {
  animal <- JMH[JMH$Location.ID == i,]
  first.row.id <- which (animal$Month == 2) [1] #find the first row of Feb in the effective year. JMH all started in Dec or Jan
  if (is.na(first.row.id)) {
    JMH.info[which(JMH.info$Location.ID == i),]$ef.mig <- NA
    next}
  first.row.time <- animal$time[first.row.id]
  last.row.time <- animal$time[nrow(animal)]
  last.supposed.time <- (animal[first.row.id, "time"] + 365*60*60*24)
  if (last.row.time < last.supposed.time) {
    if ((as.numeric(format(last.row.time, "%m")) < 4) & (as.numeric(format(last.row.time, "%Y")) ==  as.numeric(format(first.row.time, "%Y")))) {
      JMH.info[which(JMH.info$Location.ID == i),]$ef.mig <- NA
      next
    }
    else {
      animal <- animal[first.row.id:nrow(animal),]  #at least can cover one migration season, so use the data
      if (as.numeric(format(last.row.time, "%m")) == (12 | 1)) {
        JMH.info[which(JMH.info$Location.ID == i),]$ef.mig <- "Sufficient" #should be enough to cover both migration season
        JMH.info[which(JMH.info$Location.ID == i),]$ef.year <- as.numeric(format(first.row.time, "%Y"))
      }
      else {
        JMH.info[which(JMH.info$Location.ID == i),]$ef.mig <- "Spring Only"  #only spring, at least the onset, is covered (at least has data covering April)
        JMH.info[which(JMH.info$Location.ID == i),]$ef.year <- as.numeric(format(first.row.time, "%Y"))
      }
    }
  }
  else {
    last.row.id <- which(animal$time >= last.supposed.time) [1]
    animal <- animal[first.row.id:last.row.id,]
    JMH.info[which(JMH.info$Location.ID == i),]$ef.mig <- "Compelete Year"
    JMH.info[which(JMH.info$Location.ID == i),]$ef.year <- as.numeric(format(first.row.time, "%Y"))
  }
  JMH.Mig.Cycle <- rbind(JMH.Mig.Cycle, animal)
}

# write.csv(JMH.Mig.Cycle, "data/JMH.Mig.Cycle.csv")
# write.csv(JMH.info, "data/FINAL_JMHanimalTableWX.csv")
 