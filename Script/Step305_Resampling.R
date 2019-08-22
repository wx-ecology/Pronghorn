##### STEP 3.5: change data resolution (for higher convergence)
# Created: Jan 2019

# input: ("PAPO_35_Mig_Cycle.csv"), "PAPO_Mig_Cycle.csv", "FINAL_PAPOanimalTableWX.csv", "FINAL_JMHanimalTableWX.csv"
# output:("PAPO_35_Mig_Cycle_1pDay.csv"), FINAL_ALLanimalTableWX.csv

setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/data/DataForAnalysis")
# do not load library(plyr)
library(dplyr)

#prongs.info <- read.csv("FINAL_PAPOanimalTableWX.csv")

prongs <- read.csv("PAPO_35_Mig_Cycle.csv")                                                                           # change based on the dataset 
prongs$time <- as.POSIXct(strptime(as.character(prongs$time), "%Y-%m-%d %H:%M",tz = "MST"))

prongs.1pDay <- prongs %>% 
  group_by(Location.ID, Year, Month, Day) %>% 
  summarise(Easting = mean(Easting), Northing = mean(Northing))

prongs.1pDay$date <- paste0(prongs.1pDay$Year, "-", prongs.1pDay$Month, "-", prongs.1pDay$Day)
prongs.1pDay <- prongs.1pDay[, c(1,5:7)]

write.csv(prongs.1pDay, "PAPO_35_Mig_Cycle_1pDay.csv")


# try to go all PAPO ---------------
# input: ("PAPO_Mig_Cycle.csv") 
# output:("PAPO_Mig_Cycle_1pDay.csv") 

setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/data/DataForAnalysis")
# do not load library(plyr)
library(dplyr)

#prongs.info <- read.csv("FINAL_PAPOanimalTableWX.csv")

prongs <- read.csv("PAPO_Mig_Cycle.csv")                                                                           # change based on the dataset 
prongs$time <- as.POSIXct(strptime(as.character(prongs$time), "%m/%d/%y %H:%M",tz = "MST"))

prongs.1pDay <- prongs %>% 
  group_by(Location.ID, Year, Month, Day) %>% 
  summarise(Easting = mean(Easting), Northing = mean(Northing))

prongs.1pDay$date <- paste0(prongs.1pDay$Year, "-", prongs.1pDay$Month, "-", prongs.1pDay$Day)
prongs.1pDay <- prongs.1pDay[, c(1,5:7)]

write.csv(prongs.1pDay, "PAPO_Mig_Cycle_1pDay.csv")

# try to go all JMH---------------
# input: ("JMH_Mig_Cycle.csv") 
# output:("JMH_Mig_Cycle_1pDay.csv") 

setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/data/DataForAnalysis")
# do not load library(plyr)
library(dplyr)

#prongs.info <- read.csv("FINAL_PAPOanimalTableWX.csv")

prongs <- read.csv("JMH_Mig_Cycle.csv")                                                                           # change based on the dataset 
prongs$time <- as.POSIXct(strptime(as.character(prongs$time), "%m/%d/%y %H:%M",tz = "MST"))

prongs.1pDay <- prongs %>% 
  group_by(Location.ID, Year, Month, Day) %>% 
  summarise(Easting = mean(Easting), Northing = mean(Northing))

prongs.1pDay$date <- paste0(prongs.1pDay$Year, "-", prongs.1pDay$Month, "-", prongs.1pDay$Day)
prongs.1pDay <- prongs.1pDay[, c(1,5:7)]

write.csv(prongs.1pDay, "JMH_Mig_Cycle_1pDay.csv")

#put JMH and PAPO (one per day) together ----------------------
setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/data/DataForAnalysis")
prongs.papo <- read.csv("PAPO_Mig_Cycle_1pDay.csv") 
prongs.papo$date <- as.POSIXct(strptime(as.character(prongs.papo$date), "%Y-%m-%d",tz = "MST"))
prongs.papo$Location.ID <- paste0("PAPO_",prongs.papo$Location.ID)
prongs.jmh <- read.csv("JMH_Mig_Cycle_1pDay.csv") 
prongs.jmh$date <- as.POSIXct(strptime(as.character(prongs.jmh$date), "%Y-%m-%d",tz = "MST"))
prongs.jmh$Location.ID <- paste0("JMH_",prongs.jmh$Location.ID)
prongs.all <- rbind(prongs.papo, prongs.jmh)
write.csv(prongs.all, "ALLPRONGS_Mig_Cycle_1pDay.csv")

prongs.info.papo <- read.csv("FINAL_PAPOanimalTableWX.csv")
prongs.info.jmh <- read.csv("FINAL_JMHanimalTableWX.csv")
prongs.info.papo$Location.ID <- paste0("PAPO_", prongs.info.papo$Location.ID)
prongs.info.jmh$Location.ID <- paste0("JMH_", prongs.info.jmh$Location.ID)

prongs.boss <- rbind(prongs.info.papo, prongs.info.jmh)
write.csv(prongs.boss, "FINAL_ALLanimalTableWX.csv")

#if put JMH and PAPO (all data) together  ------------------------------
setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/data/DataForAnalysis")
prongs.papo <- read.csv("PAPO_Mig_Cycle.csv") 
prongs.papo$time <- as.POSIXct(strptime(as.character(prongs.papo$time), "%m/%d/%y %H:%M",tz = "MST"))
prongs.papo$Location.ID <- paste0("PAPO_",prongs.papo$Location.ID)
prongs.jmh <- read.csv("JMH_Mig_Cycle.csv") 
prongs.jmh$time <- as.POSIXct(strptime(as.character(prongs.jmh$time), "%m/%d/%y %H:%M",tz = "MST"))
prongs.jmh$Location.ID <- paste0("JMH_",prongs.jmh$Location.ID)
prongs.jmh <- prongs.jmh[,2:15]
colnames(prongs.jmh) <- colnames(prongs.papo)
prongs.all <- rbind(prongs.papo, prongs.jmh)
write.csv(prongs.all, "ALLPRONGS_Mig_Cycle.csv")
