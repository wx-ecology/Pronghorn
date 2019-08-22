##### STEP 4: calculating NSD and MSD, and use MSD for migraiton mode classification  try all PAPO+JMH
# Created: Jan 2019

# input: PAPO.Mig.Cycle // JMH.Mig.Cycle
# output: analysis/NSD/ALL_All MSD30_Step 4 Parameters // ALL All MSD 30 Output.txt"

# setup ---------------------
library(adehabitatLT)
library(adehabitatHR)
library(plotrix)
library(lattice)
library(gdata)
library(nlme)
library(dplyr)
library(plyr)
library(zoo)

setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/data/DataForAnalysis")
#prongs.info <- read.csv("FINAL_PAPOanimalTableWX.csv")
prongs.info <- read.csv("FINAL_ALLanimalTableWX.csv")


#prongs <- read.csv("PAPO_mig_Cycle.csv")
#prongs$time <- as.POSIXct(strptime(as.character(prongs$time), "%m/%d/%y %H:%M",tz = "MST"))
#prongs <- prongs[!is.na(prongs$Easting),]

# PAPO_35 <- prongs[prongs$Interval == 3.5,]
# PAPO_30 <- prongs[prongs$Interval == 3,]
# PAPO_20 <- prongs[prongs$Interval == 2,]  

# write.csv(PAPO_35, "PAPO_35_Mig_Cycle.csv")
# write.csv(PAPO_30, "PAPO_30_Mig_Cycle.csv")
# write.csv(PAPO_20, "PAPO_20_Mig_Cycle.csv")

#prongs <- read.csv("PAPO_Mig_Cycle_1pDay.csv") 
#prongs <- read.csv("JMH_Mig_Cycle_1pDay.csv")  # change based on the dataset
prongs <- read.csv("FINAL_ALL_Mig_Cycle_1pDay.csv")
prongs$date <- as.POSIXct(strptime(as.character(prongs$date), "%Y-%m-%d",tz = "MST"))

prongs <- prongs[!is.na(prongs$Easting),]

coordinates(prongs) <- c("Easting", "Northing")
proj4string(prongs) <- CRS("+init=epsg:32612")
prongs.traj <- as.ltraj(coordinates(prongs), date = prongs$date, id = prongs$Location.ID)
#regularize data
prongs.NA <- setNA(prongs.traj,min(prongs$date),1,units="day")                                                    # change based on the dataset
prongs.traj <- sett0(prongs.NA,min(prongs$date),1,units="day")                                                   # change based on the dataset

prongs.df <- ld(prongs.traj)   #this dataframe is one with added NA rows. Consistance with the traj rows
nsds <- do.call(rbind, prongs.traj)  #nsds has become a dataframe that connect the trajs

# Add capture location as one of the columns --------------
prongs.info.1 <- data.frame(prongs.info$Location.ID, prongs.info$Capture.Area) #just to subset the whole info table
colnames(prongs.info.1) <- c("id", "Cap.Loc")
prongs.info.1$id <- as.character(prongs.info.1$id)
prongs.df$id <- as.character(prongs.df$id)
prongs.df <- left_join(prongs.df, prongs.info.1, by = "id")
summary (prongs.df)

# calculating time diffences from the first day of the individual-year - FOR each individual. -------
# nDaysYr is actually the number of days since 1/2 of year year - NOT julian day
calculate.date <- function (dataframe) {
  for (i in 1:nrow(dataframe)) {
    time.diff <- (as.numeric(dataframe$date)[i] - as.numeric(dataframe$date)[1])
    time.diff <- time.diff/(24*60*60) #convert to digits day
    #first.day <- as.numeric(format(dataframe$date,"%j")) [1]
    dataframe$nDaysYr[i] <- #first.day + 
      time.diff
  }
  dataframe$nDaysYr  #return a vector
}

nDaysYr <- vector()
for (i in unique(prongs.df$id)) {
  prongs.df.1 <- prongs.df[prongs.df$id == i,]
  nDaysYr <- c(nDaysYr, calculate.date(prongs.df.1))  
}
prongs.df$nDaysYr <- nDaysYr


## calculating NSD ---------------------------
prongs.df$NSD <-nsds$R2n/1000000

#####################
# get rid of the ids that does not have enough data ----
bad.id.na <- as.character(prongs.info[is.na(prongs.info$ef.mig), ]$Location.ID)  # the ids of the invidiuals have na ef.mig
bad.id.spring <- as.character((prongs.info[prongs.info$ef.mig == "Spring Only", ]$Location.ID)[!is.na(prongs.info[prongs.info$ef.mig == "Spring Only", ]$Location.ID)])
bad.id <- as.character(c(bad.id.na, bad.id.spring))

for (i in unique(prongs.df$id)) {
  if (i %in% bad.id) {
    prongs.df <- prongs.df[prongs.df$id != i, ]
  }
}

## Estimate MSD with (of your own choice) time window ---------------------
# Singh 2016 used step length of 30     #might change the window size if cannot converge

steps <- 30      #seems like 10 is too small and will cause error, but 15 is good. 
prongs.df.1 <- ddply(prongs.df, .(id), function(prongs.df) {
  z <- zoo(prongs.df$NSD, prongs.df$date)
  prongs.df$rollmean <- rollapply(z, width=steps, align = "right", partial=TRUE, FUN=mean, na.rm=TRUE) 
  prongs.df
})
prongs.df$MSD <- coredata(prongs.df.1$rollmean)

#####################

# summary(prongs.df)
# write.csv (prongs.df, paste0("JMH_all_1perDayNSD_", steps, "dMSD.csv"))
# write.csv (prongs.df, paste0("ALL_1perDayNSD_", steps, "dMSD.csv"))

#prongs.df.origin <- prongs.df  #for calculation purposes
#prongs.df <- prongs.df.origin

# # # # # plot NSD ---------------------
# library (ggplot2)
# library (scales)
# #  color by id
# prongs.df$Date <- as.Date(prongs.df$date)
# plot2<-qplot(Date, sqrt(NSD), data = prongs.df, geom = "path", colour = id)
# 
# Plot2 <- plot2 +  geom_path(size = 0.7) +
#   theme(axis.title.y = element_text(size = 12, vjust = 0.3, face = "bold")) +
#   theme(axis.title.x = element_text(size = 12, vjust = 0.5, face = "bold")) +
#   scale_x_date(labels = date_format("%b")) +
#   theme(panel.border = element_rect(colour = "black", fill = "NA")) +
#   theme(panel.background = element_rect(fill = "white")) +
#   theme(panel.grid.minor.x = element_line(colour="white")) +
#   theme(panel.grid.minor.y = element_line(colour="white")) + xlab("Date") + ylab("Distance (km)") +
#   theme(axis.text.y = element_text(face = "bold", size = rel(1.3))) +
#   theme(legend.position="none") +
#   theme(axis.text.x = element_text(face = "bold", size = rel(1.3)))
# # by id, all individuals together
# Plot2
# # by id, for each individual
# Plot2 + facet_wrap(~id, scales = "free")
# 
# # color by location
# plot3<-ggplot(prongs.df, aes(Date, sqrt(NSD), group = id, colour = Cap.Loc)) + geom_path()
# Plot3 <- plot3 +  geom_path(size = 0.7) +
#   theme(axis.title.y = element_text(size = 12, vjust = 0.3, face = "bold")) +
#   theme(axis.title.x = element_text(size = 12, vjust = 0.5, face = "bold")) +
#   scale_x_date(labels = date_format("%b")) +
#   theme(panel.border = element_rect(colour = "black", fill = "NA")) +
#   theme(panel.background = element_rect(fill = "white")) +
#   theme(panel.grid.minor.x = element_line(colour="white")) +
#   theme(panel.grid.minor.y = element_line(colour="white")) + xlab("Date") + ylab("Distance (km)") +
#   theme(axis.text.y = element_text(face = "bold", size = rel(1.3))) +
#   theme(axis.text.x = element_text(face = "bold", size = rel(1.3)))
# Plot3
# 
# #plot with day from Feb
# plot4 <-qplot(nDaysYr, sqrt(NSD), data = prongs.df, geom = "path", colour = id)
# Plot4 <- plot4 +  geom_path(size = 0.7) +
#   theme(axis.title.y = element_text(size = 12, vjust = 0.3, face = "bold")) +
#   theme(axis.title.x = element_text(size = 12, vjust = 0.5, face = "bold")) +
# #  scale_x_date(labels = date_format("%b")) +
#   theme(panel.border = element_rect(colour = "black", fill = "NA")) +
#   theme(panel.background = element_rect(fill = "white")) +
#   theme(panel.grid.minor.x = element_line(colour="white")) +
#   theme(panel.grid.minor.y = element_line(colour="white")) + xlab("Date") + ylab("Distance (km)") +
#   theme(axis.text.y = element_text(face = "bold", size = rel(1.3))) +
#   theme(legend.position="none") +
#   theme(axis.text.x = element_text(face = "bold", size = rel(1.3)))
# # by id, all individuals together
# Plot4
# 
# #plot with day from Feb, colar by year
# plot5 <-ggplot(prongs.df, aes(nDaysYr, sqrt(NSD), group = id, colour = format(date, "%Y"))) + geom_path()
# Plot5 <- plot5 +  geom_path(size = 0.7) +
#   theme(axis.title.y = element_text(size = 12, vjust = 0.3, face = "bold")) +
#   theme(axis.title.x = element_text(size = 12, vjust = 0.5, face = "bold")) +
#   #  scale_x_date(labels = date_format("%b")) +
#   theme(panel.border = element_rect(colour = "black", fill = "NA")) +
#   theme(panel.background = element_rect(fill = "white")) +
#   theme(panel.grid.minor.x = element_line(colour="white")) +
#   theme(panel.grid.minor.y = element_line(colour="white")) + xlab("Date") + ylab("Distance (km)") +
#   theme(axis.text.y = element_text(face = "bold", size = rel(1.3))) +
#   theme(axis.text.x = element_text(face = "bold", size = rel(1.3)))
# # by id, all individuals together
# Plot5

####################################################################################
####################################################################################
# here starts the modeling part. Reading in propor dataset from here --------------------------
####################################################################################
####################################################################################
# read.csv(xxxxxxx)


# fit each individual to each of the movement mode model ----------------------

###Now we fit each of the models described in Bunnefeld et al. 2011 and Singh et al. 2012
###null model ------------------
## PAPO 15, 30 ok, JMH 30 OK, ALL 30 OK
null.HRmod <- nlme(MSD ~ A,
                   data = prongs.df,
                   fixed = A ~ 1,
                   random = A ~ 1,
                   groups = ~ id,
                   start = c(A = mean (prongs.df[,'MSD'])))

### change starting values if no conv (=mean), such as a specific value, or median etc.

###Sedentary HR model ------------------
## HR model ####################################
## PAPO 15, 30 ok , JMH 30 OK,  ALL 30 OK
asym.HRmod <- nlme(MSD ~ Asym*(1 - exp(lrc*nDaysYr)),
                   data = prongs.df,
                   fixed = Asym + lrc ~ 1,
                   random = Asym ~ 1,
                   groups = ~ id,
                   start = c(Asym = summary(null.HRmod)$tTable[1], 
                                 lrc = -0.002))

##Dispersal model1 ------------------
#with distance (Asym) and timing (xmid) varying between individuals. 
##Providing a start improve model convergence, which can also be obtained by using parameters from previously estimated models
#######Dispersal model #################################################
## PAPO 15, 30 ok, JMH 30 OK,   ALL 30 OK
ranef2.Dispmod <- nlme(MSD ~ Asym/(1 + exp((xmid-nDaysYr)/scal)),
                       data = prongs.df,
                       fixed = Asym + xmid + scal ~ 1,
                       random = Asym + xmid ~ 1,
                       groups = ~ id,
                       na.action = na.exclude,
                       # #verbose = TRUE,
                       start = c(Asym = summary(asym.HRmod)$tTable[1], xmid = 30, scal = 2)
                       )
summary(ranef2.Dispmod)

##Dispersal model 2 ------------------
#with Distance, timing and duration (scal) varying between individuals. 
############Dispersal model 2 ############################################
# papo step 15 fail, 30 fail, JMH 30 OK, all 30 fail
# full.Dispmod <- nlme(MSD ~ Asym/(1 + exp((xmid-nDaysYr)/scal)),
#                      data = prongs.df,
#                      fixed = Asym + xmid + scal ~ 1,
#                      random = Asym + xmid + scal ~ 1,
#                      groups = ~ id,
#                      # verbose = TRUE,
#                      start = c(Asym = 12000,
#                                xmid = 200, scal = 40))
#                      # start = c(Asym = summary(ranef2.Dispmod)$tTable[1],
#                      #           xmid = summary(ranef2.Dispmod)$tTable[2], scal = summary(ranef2.Dispmod)$tTable[3]))

# # #summary(full.Dispmod)

##Simple Migration model# mig model 2 ------------------
#with only distance varying between individuals
#########Simple Mig model ###############################################
## PAPO 15, 30 ok, JMH 30 OK, all 30 ok
asym.Migrmod <- nlme(MSD ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
                     data=prongs.df,
                     fixed = asym+ xmidA + xmidB + scale1 + scale2 ~ 1,
                     random = asym ~ 1,
                     groups =~ id,
                     start = c(asym = 2000,xmidA = 70, xmidB = 300, scale1 = 17, scale2 = 60))
summary(asym.Migrmod)

## Mig Model 2: More complex migration model----------
## with distance, timing of Migration 1 (xmidA) and timing of migration 2 (xmidB) varying with individual
##keep trying to change the start value
## 15, 30 fail, JMH 30 fail, all 30 fail
# ranef2.Migrmod <- nlme(MSD ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
#                        data=prongs.df,
#                        fixed = asym + xmidA + xmidB + scale1 + scale2 ~ 1,
#                        random = asym + xmidA + xmidB ~ 1|id,
#                        #groups =~ id, #control=contr,
#                        start = c(asym = 4000, xmidA = 110,
#                                  xmidB = 300, scale1 = 15, scale2 = 20))
# summary(ranef2.Migrmod)

# ##Complete migration model ----------
# ##with all parameters varying between individuals. Normally this will struggle to converge in a dataset
# ##that contain a mix of migrants and non-migrants, and is more important in later stages when estimating timing of movements for 
# ##migratory individuals only using the NSD rather than the MSD 
# PAPO step 20/30 cannot converge - but it is ok for this stage
# ranef4.Migrmod <- nlme(MSD ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
#                        data=prongs.df,
#                        fixed = list(asym + xmidA + xmidB + scale1 + scale2 ~ 1),
#                        random = asym + xmidA + xmidB + scale1 + scale2 ~ 1|id,
#                        #groups =~ id, control=contr,
#                        start = c(asym = summary(ranef2.Migrmod)$tTable[1], xmidA = summary(ranef2.Migrmod)$tTable[2],
#                                  xmidB = summary(ranef2.Migrmod)$tTable[3], scale1 = 5,
#                                  scale2 = 10))
# summary(ranef4.Migrmod)

#Mixed Migratory Model ----------
#with distance and timing of Migration 1, and distance and timing of Migration 2, varying between individuals.
# PAPO 15，30 OK, JMH 30 OK, ALL 30 success with warnings
mix.Migrmod <- nlme(MSD ~ asymA/(1+exp((xmidA-nDaysYr)/scale1)) + (-asymB /(1 + exp((xmidB-nDaysYr)/scale2))),
                    data=prongs.df,
                    fixed = asymA + asymB + xmidA + xmidB + scale1 + scale2 ~ 1,
                    random = asymA + asymB +xmidA + xmidB~ 1,
                    groups =~ id,
                    start = c(asymA = 2000, asymB = 1000, xmidA = 100, xmidB = 300, scale1 = 17, scale2 = 50))
                    # start = c(asymA = 10000, asymB = 8000, xmidA = 110, xmidB = 300, scale1 = 8, scale2 = 12))
summary(mix.Migrmod)

##Nomadic model
######## Nomadic model #############################################################################################
# PAPO 15 fail 30 (D=2), JMH 30 (D=1), ALL 30 (D=1) success
all.linear <- nlme(MSD ~ 4*D*nDaysYr,
                   data = prongs.df,
                   fixed = D ~ 1,
                   random = D ~ 1,
                   groups = ~ id,
                   start = c(D = 1))

### GOF indiv level --------------------
### change this list below to include the model that fitted (with highest number of random effects) for each movement type

#change this according to which model succeed 
HRmod <- asym.HRmod #
NullMod <- null.HRmod
Dispmod <- ranef2.Dispmod #full.Dispmod for JMH only
MigrMod <- asym.Migrmod #can only fit a simple mig model at this stage 
MigrModC <-mix.Migrmod
nomadMod <- all.linear #
###Now perform the calculations for the goodness of fit (i.e. concordance criterion). 
### CC1-IDlev
########################################################

# check first that id levels are the same
prongs.df$id <- as.factor(prongs.df$id)
all.equal(levels(prongs.df$id),rownames(coef(HRmod)))
all.equal(levels(prongs.df$id),rownames(coef(NullMod)))
all.equal(levels(prongs.df$id),rownames(coef(Dispmod)))
all.equal(levels(prongs.df$id),rownames(coef(MigrMod)))
all.equal(levels(prongs.df$id),rownames(coef(MigrModC)))
all.equal(levels(prongs.df$id),rownames(coef(nomadMod)))
#[1] TRUE    # same for Dispmod etc.


###
#HR----------
###
CC1ID.HRmod <- numeric(length(levels(prongs.df$id)))

for(k in 1:length(levels(prongs.df$id))) {
  
  CC1ID.HRmod[k] <- 1 - (sum((prongs.df[which(prongs.df$id == levels(prongs.df$id)[k]),'MSD'] -
                                fitted(HRmod)[which(prongs.df$id ==
                                                      levels(prongs.df$id)[k])])^2)) / (
                                                        (sum((prongs.df[which(prongs.df$id == levels(prongs.df$id)[k]),'MSD'] -
                                                                mean(prongs.df[which(prongs.df$id ==
                                                                                       levels(prongs.df$id)[k]),'MSD']))^2)) +
                                                          (sum((fitted(HRmod)[which(prongs.df$id == levels(prongs.df$id)[k])] -
                                                                  mean(fitted(HRmod)[which(prongs.df$id ==
                                                                                             levels(prongs.df$id)[k])]))^2)) +
                                                          (length(prongs.df[which(prongs.df$id ==
                                                                                    levels(prongs.df$id)[k]),'MSD'])*((mean(prongs.df[which(prongs.df$id==levels(prongs.df$id)[k]),'MSD'])
                                                                                                                              -
                                                                                                                                mean(fitted(HRmod)[which(prongs.df$id ==
                                                                                                                                                           levels(prongs.df$id)[k])]))^2))
                                                      )
}
###
#NULL----------
##
CC1ID.NullMod <- numeric(length(levels(prongs.df$id)))

for(k in 1:length(levels(prongs.df$id))) {
  
  CC1ID.NullMod[k] <- 1 - (sum((prongs.df[which(prongs.df$id == levels(prongs.df$id)[k]),'MSD'] -
                                  fitted(NullMod)[which(prongs.df$id ==
                                                          levels(prongs.df$id)[k])])^2)) / (
                                                            (sum((prongs.df[which(prongs.df$id == levels(prongs.df$id)[k]),'MSD'] -
                                                                    mean(prongs.df[which(prongs.df$id ==
                                                                                           levels(prongs.df$id)[k]),'MSD']))^2)) +
                                                              (sum((fitted(NullMod)[which(prongs.df$id == levels(prongs.df$id)[k])] -
                                                                      mean(fitted(NullMod)[which(prongs.df$id ==
                                                                                                   levels(prongs.df$id)[k])]))^2)) +
                                                              (length(prongs.df[which(prongs.df$id ==
                                                                                        levels(prongs.df$id)[k]),'MSD'])*((mean(prongs.df[which(prongs.df$id==levels(prongs.df$id)[k]),'MSD'])
                                                                                                                                  -
                                                                                                                                    mean(fitted(NullMod)[which(prongs.df$id ==
                                                                                                                                                                 levels(prongs.df$id)[k])]))^2))
                                                          )
}

###
#Dispersal----------
##
CC1ID.Dispmod <- numeric(length(levels(prongs.df$id)))

for(k in 1:length(levels(prongs.df$id))) {
  
  CC1ID.Dispmod[k] <- 1 - (sum((prongs.df[which(prongs.df$id == levels(prongs.df$id)[k]),'MSD'] -
                                  fitted(Dispmod)[which(prongs.df$id ==
                                                          levels(prongs.df$id)[k])])^2)) / (
                                                            (sum((prongs.df[which(prongs.df$id == levels(prongs.df$id)[k]),'MSD'] -
                                                                    mean(prongs.df[which(prongs.df$id ==
                                                                                           levels(prongs.df$id)[k]),'MSD']))^2)) +
                                                              (sum((fitted(Dispmod)[which(prongs.df$id == levels(prongs.df$id)[k])] -
                                                                      mean(fitted(Dispmod)[which(prongs.df$id ==
                                                                                                   levels(prongs.df$id)[k])]))^2)) +
                                                              (length(prongs.df[which(prongs.df$id ==
                                                                                        levels(prongs.df$id)[k]),'MSD'])*((mean(prongs.df[which(prongs.df$id==levels(prongs.df$id)[k]),'MSD'])
                                                                                                                                  -
                                                                                                                                    mean(fitted(Dispmod)[which(prongs.df$id ==
                                                                                                                                                                 levels(prongs.df$id)[k])]))^2))
                                                          )
}

###
#Migration----------
###
CC1ID.MigrMod <- numeric(length(levels(prongs.df$id)))

for(k in 1:length(levels(prongs.df$id))) {
  
  CC1ID.MigrMod[k] <- 1 - (sum((prongs.df[which(prongs.df$id == levels(prongs.df$id)[k]),'MSD'] -
                                  fitted(MigrMod)[which(prongs.df$id ==
                                                          levels(prongs.df$id)[k])])^2)) / (
                                                            (sum((prongs.df[which(prongs.df$id == levels(prongs.df$id)[k]),'MSD'] -
                                                                    mean(prongs.df[which(prongs.df$id ==
                                                                                           levels(prongs.df$id)[k]),'MSD']))^2)) +
                                                              (sum((fitted(MigrMod)[which(prongs.df$id == levels(prongs.df$id)[k])] -
                                                                      mean(fitted(MigrMod)[which(prongs.df$id ==
                                                                                                   levels(prongs.df$id)[k])]))^2)) +
                                                              (length(prongs.df[which(prongs.df$id ==
                                                                                        levels(prongs.df$id)[k]),'MSD'])*((mean(prongs.df[which(prongs.df$id==levels(prongs.df$id)[k]),'MSD'])
                                                                                                                                  -
                                                                                                                                    mean(fitted(MigrMod)[which(prongs.df$id ==
                                                                                                                                                                 levels(prongs.df$id)[k])]))^2))
                                                          )
}

###
#MigrationC----------
###
CC1ID.MigrModC <- numeric(length(levels(prongs.df$id)))

for(k in 1:length(levels(prongs.df$id))) {
  
  CC1ID.MigrModC[k] <- 1 - (sum((prongs.df[which(prongs.df$id == levels(prongs.df$id)[k]),'MSD'] -
                                   fitted(MigrModC)[which(prongs.df$id ==
                                                            levels(prongs.df$id)[k])])^2)) / (
                                                              (sum((prongs.df[which(prongs.df$id == levels(prongs.df$id)[k]),'MSD'] -
                                                                      mean(prongs.df[which(prongs.df$id ==
                                                                                             levels(prongs.df$id)[k]),'MSD']))^2)) +
                                                                (sum((fitted(MigrModC)[which(prongs.df$id == levels(prongs.df$id)[k])] -
                                                                        mean(fitted(MigrModC)[which(prongs.df$id ==
                                                                                                      levels(prongs.df$id)[k])]))^2)) +
                                                                (length(prongs.df[which(prongs.df$id ==
                                                                                          levels(prongs.df$id)[k]),'MSD'])*((mean(prongs.df[which(prongs.df$id==levels(prongs.df$id)[k]),'MSD'])
                                                                                                                                    -
                                                                                                                                      mean(fitted(MigrModC)[which(prongs.df$id ==
                                                                                                                                                                    levels(prongs.df$id)[k])]))^2))
                                                            )
}

###
#Nomad----------
###
CC1ID.nomadMod <- numeric(length(levels(prongs.df$id)))

for(k in 1:length(levels(prongs.df$id))) {
  
  CC1ID.nomadMod[k] <- 1 - (sum((prongs.df[which(prongs.df$id == levels(prongs.df$id)[k]),'MSD'] -
                                   fitted(nomadMod)[which(prongs.df$id ==
                                                            levels(prongs.df$id)[k])])^2)) / (
                                                              (sum((prongs.df[which(prongs.df$id == levels(prongs.df$id)[k]),'MSD'] -
                                                                      mean(prongs.df[which(prongs.df$id ==
                                                                                             levels(prongs.df$id)[k]),'MSD']))^2)) +
                                                                (sum((fitted(nomadMod)[which(prongs.df$id == levels(prongs.df$id)[k])] -
                                                                        mean(fitted(nomadMod)[which(prongs.df$id ==
                                                                                                      levels(prongs.df$id)[k])]))^2)) +
                                                                (length(prongs.df[which(prongs.df$id ==
                                                                                          levels(prongs.df$id)[k]),'MSD'])*((mean(prongs.df[which(prongs.df$id==levels(prongs.df$id)[k]),'MSD'])
                                                                                                                                    -
                                                                                                                                      mean(fitted(nomadMod)[which(prongs.df$id ==
                                                                                                                                                                    levels(prongs.df$id)[k])]))^2))
                                                            )
}

###############################################
###Create a dataframe of the results----------
SpaceUseClass <- data.frame(CC1ID.HRmod = CC1ID.HRmod,CC1ID.NullMod = CC1ID.NullMod,CC1ID.Dispmod = CC1ID.Dispmod,
                            CC1ID.MigrMod = CC1ID.MigrMod,CC1ID.MigrModC = CC1ID.MigrModC,CC1ID.nomadMod = CC1ID.nomadMod, id = levels(prongs.df$id))

### add column with space use classification according to highest CC1 value
maxCC1ID <- (apply(SpaceUseClass[,1:6], 1, which.max))
SpaceUseClass$bestMod.CC1ID <- factor(ifelse(maxCC1ID == 1, "HRmod", ifelse(maxCC1ID == 2, "NullMod", ifelse(maxCC1ID == 3, "Dispmod",
                                                                                                             ifelse(maxCC1ID == 4, "MigrMod", ifelse(maxCC1ID == 5, "MigrModC", ifelse(maxCC1ID == 6, "nomadMod", NA)))))))

###Save results of movement classifications and goodness of fits.  
coeffs<-coef(MigrModC)
params<-data.frame(coeffs,SpaceUseClass)
setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/NSD")
# sink(paste0("PAPO_All MSD", steps, " Output.txt"))
# sink(paste0("JMH_All MSD", steps, " Output.txt"))
sink(paste0("ALL All MSD", steps, " Output0121.txt"))
summary(MigrModC)
sink()
#write.csv(params,paste0("PAPO_All MSD", steps, "Step4 Parameters.csv"))
#write.csv(params,paste0("JMH_All MSD", steps, "Step4 Parameters.csv"))
write.csv(params,paste0("ALL_All MSD", steps, "Step4 Parameters0121.csv"))


#################################################
#################################################
#################################################
# little additional section -- calculate NSD for ALL data by time interval with their original intervals-------------------
#################################################

setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/data/DataForAnalysis")
prongs.info <- read.csv("FINAL_ALLanimalTableWX.csv")

prongs <- read.csv("PAPO_30_Mig_Cycle.csv")
prongs$date <- as.POSIXct(strptime(as.character(prongs$time), "%Y-%m-%d %H:%M",tz = "MST"))

prongs <- prongs[!is.na(prongs$Easting),]

coordinates(prongs) <- c("Easting", "Northing")
proj4string(prongs) <- CRS("+init=epsg:32612")
prongs.traj <- as.ltraj(coordinates(prongs), date = prongs$date, id = prongs$Location.ID)
#regularize data
prongs.NA <- setNA(prongs.traj,min(prongs$date),3,units="hour")                                                    # change based on the dataset
prongs.traj <- sett0(prongs.NA,min(prongs$date),3,units="hour")                                                   # change based on the dataset

prongs.df <- ld(prongs.traj)   #this dataframe is one with added NA rows. Consistance with the traj rows
nsds <- do.call(rbind, prongs.traj)  #nsds has become a dataframe that connect the trajs

# calculating time diffences from the first day of the individual-year - FOR each individual. -------
# nDaysYr is actually the number of days since 1/2 of year year - NOT julian day
calculate.date <- function (dataframe) {
  for (i in 1:nrow(dataframe)) {
    time.diff <- (as.numeric(dataframe$date)[i] - as.numeric(dataframe$date)[1])
    time.diff <- time.diff/(24*60*60) #convert to digits day
    #first.day <- as.numeric(format(dataframe$date,"%j")) [1]
    dataframe$nDaysYr[i] <- #first.day + 
      time.diff
  }
  dataframe$nDaysYr  #return a vector
}

nDaysYr <- vector()
for (i in unique(prongs.df$id)) {
  prongs.df.1 <- prongs.df[prongs.df$id == i,]
  nDaysYr <- c(nDaysYr, calculate.date(prongs.df.1))  
}
prongs.df$nDaysYr <- nDaysYr

## calculating NSD ---------------------------
prongs.df$NSD <-nsds$R2n/1000000

# change ID
prongs.df$id <- paste0("PAPO_", as.character(prongs.df$id))                                          # change based on the dataset
#####################
# get rid of the ids that does not have enough data ----
bad.id.na <- as.character(prongs.info[is.na(prongs.info$ef.mig), ]$Location.ID)  # the ids of the invidiuals have na ef.mig
bad.id.spring <- as.character((prongs.info[prongs.info$ef.mig == "Spring Only", ]$Location.ID)[!is.na(prongs.info[prongs.info$ef.mig == "Spring Only", ]$Location.ID)])
bad.id <- as.character(c(bad.id.na, bad.id.spring))

for (i in unique(prongs.df$id)) {
  if (i %in% bad.id) {
    prongs.df <- prongs.df[prongs.df$id != i, ]
  }
}


# Add capture location as one of the columns --------------
prongs.info.1 <- data.frame(prongs.info$Location.ID, prongs.info$Capture.Area) #just to subset the whole info table
colnames(prongs.info.1) <- c("id", "Cap.Loc")
prongs.info.1$id <- as.character(prongs.info.1$id)
prongs.df <- left_join(prongs.df, prongs.info.1, by = "id")
summary (prongs.df)

## Estimate MSD with (of your own choice) time window ---------------------
# Singh 2016 used step length of 30     #might change the window size if cannot converge

# steps <- 30      #seems like 10 is too small and will cause error, but 15 is good. 
# prongs.df.1 <- ddply(prongs.df, .(id), function(prongs.df) {
#   z <- zoo(prongs.df$NSD, prongs.df$date)
#   prongs.df$rollmean <- rollapply(z, width=steps, align = "right", partial=TRUE, FUN=mean, na.rm=TRUE) 
#   prongs.df
# })
# prongs.df$MSD <- coredata(prongs.df.1$rollmean)

#####################

# summary(prongs.df)
write.csv (prongs.df, paste0("PAPO_30_NSD.csv"))
