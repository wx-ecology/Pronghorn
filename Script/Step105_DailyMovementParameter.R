##### STEP 1.5 Generate traj for basic movement parameters 
# GOAL: calculate daily moving distance in migration season and in home range season for Qiongyu and Turner random walk project 

setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis")
library(adehabitatLT)
library(dplyr)
library(circular)
library(plotrix)

##  first calculate average migration timing based on the NSD result  ----- 
prongs.NSD <- read.csv("data/DataForAnalysis/FINAL_ALLanimalTableWX_wNSD.csv")

prongs.NSD$Mig.seasonA <- as.POSIXct(strptime("02-01", format = "%m-%d")) +  prongs.NSD$Mig.timingA *60*60*24 
prongs.NSD$Mig.seasonB <- as.POSIXct(strptime("02-01", format = "%m-%d")) +  prongs.NSD$Mig.timingB *60*60*24 

# write.csv(prongs.NSD, "data/DataForAnalysis/ALLanimalTableWX_wNSDparameter_forPlotting.csv")
prongs.NSD <- read.csv("data/DataForAnalysis/ALLanimalTableWX_wNSDparameter_forPlotting.csv")

prongs.NSD$Mig.seasonA <- as.Date(prongs.NSD$Mig.seasonA)
prongs.NSD$Mig.seasonB <- as.Date(prongs.NSD$Mig.seasonB)

# Create data
data <- prongs.NSD
# Reorder data 
data <- data %>% rowwise() %>% arrange(Mig.seasonA) %>% mutate(Location.ID=factor(Location.ID, Location.ID))
data <- data[1:125, ] #get rid of the outlier
data$Capture.Location <- as.character(data$Capture.Location)
for (i in 1:nrow(data)) {
  if (startsWith(as.character(data$Location.ID[i]), "JMH")) {
    data$Capture.Location[i] <- "Rock Springs"
  }
}

data$ef.year <- as.factor(data$ef.year)

# We only want PAPO dataset for the project
data <- data[startsWith(as.character(data$Location.ID), "PAPO"),]

Ave.Mig.SeasonA <- format(as.POSIXct(strptime(as.character(data$Mig.seasonA), "%Y-%m-%d")), "%j")
Ave.Mig.SeasonA <- mean(as.numeric(Ave.Mig.SeasonA))

Ave.Mig.SeasonB <- format(as.POSIXct(strptime(as.character(data$Mig.seasonB), "%Y-%m-%d")), "%j")
Ave.Mig.SeasonB <- mean(as.numeric(Ave.Mig.SeasonB))

# generate two (three) dataframe: Migration season ; HR season 1, HR season 2

PAPO <- read.csv("data/FINAL_PAPOlocationTableWX.csv")
PAPO$time <- as.POSIXct(strptime(as.character(PAPO$time), "%Y-%m-%d %H:%M"))
PAPO$time.jday <- as.numeric(format(PAPO$time, "%j"))

HR.1 <- PAPO[PAPO$time.jday < Ave.Mig.SeasonA, ]
HR.2 <- PAPO[PAPO$time.jday > Ave.Mig.SeasonB, ]
Mig <- PAPO[(PAPO$time.jday > Ave.Mig.SeasonA) & (PAPO$time.jday < Ave.Mig.SeasonB), ]

HR.1.traj <- as.ltraj(xy = HR.1[, c("Easting", "Northing")], date = HR.1$time, id = HR.1$Location.ID)
HR.2.traj <- as.ltraj(xy = HR.2[, c("Easting", "Northing")], date = HR.2$time, id = HR.2$Location.ID)
Mig.traj <- as.ltraj(xy = Mig[, c("Easting", "Northing")], date = Mig$time, id = Mig$Location.ID)


# since intervals are 2, 3, and 3.5, on average about 8 stamps make one day. 

aver <- 8

dist.list <- vector()
for (i in 1:153) {
  dist.list.i <- HR.1.traj[[i]]$dist
  dist.list.i <- dist.list.i[!is.na(dist.list.i)]
  dist.list <- c(dist.list, dist.list.i)
}
a <- dist.list

dist.list <- vector()
for (i in 1:145) {
  dist.list.i <- HR.2.traj[[i]]$dist
  dist.list.i <- dist.list.i[!is.na(dist.list.i)]
  dist.list <- c(dist.list, dist.list.i)
}
b <- dist.list

dist.list <- vector()
for (i in 1:138) {
  dist.list.i <- Mig.traj[[i]]$dist
  dist.list.i <- dist.list.i[!is.na(dist.list.i)]
  dist.list <- c(dist.list, dist.list.i)
}
c <- dist.list

mean.dist
mean(a)
mean(b)
mean(c)
std.error(a)
std.error(b)
std.error(c)

abc <- c(a,b,c)
mean(abc) * 8
std.error(abc)

# now we calculate turning angle Von Mises mean and concentration parameter based on relative angles
angle.list <- vector()
for (i in 1:153) {
  angle.list.i <- HR.1.traj[[i]]$rel.angle
  angle.list.i <- angle.list.i[!is.na(angle.list.i)]
  angle.list <- c(angle.list, angle.list.i)
}
x <- angle.list 
mle.vonmises(x)

angle.list <- vector()
for (i in 1:145) {
  angle.list.i <- HR.2.traj[[i]]$rel.angle
  angle.list.i <- angle.list.i[!is.na(angle.list.i)]
  angle.list <- c(angle.list, angle.list.i)
}
y <- angle.list 
mle.vonmises(y)

xy <- c(x,y)
mle.vonmises(xy)

angle.list <- vector()
for (i in 1:138) {
  angle.list.i <- Mig.traj[[i]]$rel.angle
  angle.list.i <- angle.list.i[!is.na(angle.list.i)]
  angle.list <- c(angle.list, angle.list.i)
}
z <- angle.list 
mle.vonmises(z)



suppressWarnings(RNGversion("3.5.0"))
set.seed(876)
u <- simm.crw(1:500, r = 0.9, burst = "h=1")
v <- simm.crw(1:500, r = 0.9, burst = "h=2", h = 2)
w <- simm.crw(1:500, r = 0.9, burst = "h=5", h = 5)
x <- simm.crw(1:500, r = 0.9, burst = "h = 0.1",h = 0.1)
z <- c(u, v, w, x)
plot(z, addpoints = FALSE, perani = FALSE)

