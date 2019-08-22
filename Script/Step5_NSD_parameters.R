######### step 5: NSD for migraiton parameters - all data that is classified as migration 
# Created: Jan 2019

# input: ALL_All MSD30_Step 4 Parameters.csv
# output
# step up ---------------
library(nlme)

setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis")
prongs.info <- read.csv("data/DataForAnalysis/FINAL_ALLanimalTableWX.csv")
prongs.class <- read.csv("NSD/ALL_All MSD30Step4 Parameters.csv")

good.id.mig <- as.character(prongs.class[prongs.class$bestMod.CC1ID == "MigrMod",]$id)
good.id.migc <- as.character(prongs.class[prongs.class$bestMod.CC1ID == "MigrModC",]$id)
good.id <- as.character(c(good.id.mig, good.id.migc))


#################################################
# 1 per day NSD, all MigC and Mig ----------------
#################################################
prongs.all <- read.csv("data/DataForAnalysis/ALL_1perDayNSD_30dMSD.csv")

prongs.mig <- data.frame()
for (i in unique(prongs.all$id)) {
  if (i %in% good.id) {
    prongs.mig <- rbind(prongs.mig, prongs.all[prongs.all$id == i, ])
  }
}

prongs.df <- prongs.mig
#Mixed Migratory Model ----------
#with distance and timing of Migration 1, and distance and timing of Migration 2, varying between individuals.
mix.Migrmod <- nlme(NSD ~ asymA/(1+exp((xmidA-nDaysYr)/scale1)) + (-asymB /(1 + exp((xmidB-nDaysYr)/scale2))),
                    data=prongs.df,
                    fixed = asymA + asymB + xmidA + xmidB + scale1 + scale2 ~ 1,
                    random = asymA + asymB +xmidA + xmidB~ 1,
                    groups =~ id,
                    start = c(asymA = 10000, asymB = 8000, xmidA = 100, xmidB = 300, scale1 = 10, scale2 = 8))
# start = c(asymA = 10000, asymB = 8000, xmidA = 110, xmidB = 300, scale1 = 8, scale2 = 12))
summary(mix.Migrmod)

##Simple Migration model# mig model 2 ------------------
#with only distance varying between individuals
#########Simple Mig model ###############################################
# asym.Migrmod <- nlme(NSD ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
#                      data=prongs.df,
#                      fixed = asym+ xmidA + xmidB + scale1 + scale2 ~ 1,
#                      random = asym ~ 1,
#                      groups =~ id,
#                      start = c(asym = 5000, xmidA = 100, xmidB = 300, scale1 = 10, scale2 = 8))
# summary(asym.Migrmod)

# Mig Model 2: More complex migration model----------
# with distance, timing of Migration 1 (xmidA) and timing of migration 2 (xmidB) varying with individual
# keep trying to change the start value
# cannot converge (maximum iterations reached)
# ranef2.Migrmod <- nlme(NSD ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
#                        data=prongs.df,
#                        fixed = asym + xmidA + xmidB + scale1 + scale2 ~ 1,
#                        random = asym + xmidA + xmidB ~ 1,
#                        groups =~ id, #control=contr,
#                        start = c(asym = 2184.5595, xmidA = 88.9819,
#                                  xmidB = 259.4411, scale1 = 11.0841, scale2 = 14.1692))
# summary(ranef2.Migrmod)

# ##Complete migration model ----------
# ##with all parameters varying between individuals. Normally this will struggle to converge in a dataset
# ##that contain a mix of migrants and non-migrants, and is more important in later stages when estimating timing of movements for 
# ##migratory individuals only using the NSD rather than the MSD 
# keep trying making the parameters larger.
ranef4.Migrmod <- nlme(NSD ~ asym/(1+exp((xmidA-nDaysYr)/scale1)) + (-asym /(1 + exp((xmidB-nDaysYr)/scale2))),
                       data=prongs.df,
                       fixed = list(asym + xmidA + xmidB + scale1 + scale2 ~ 1),
                       random = asym + xmidA + xmidB + scale1 + scale2 ~ 1|id,
                       #groups =~ id, control=contr,
                       start = c(asym = 12000, xmidA = 120, xmidB = 300, scale1 = 15, scale2 = 8))
                       # start = c(asym = summary(ranef2.Migrmod)$tTable[1], xmidA = summary(ranef2.Migrmod)$tTable[2],
                       #           xmidB = summary(ranef2.Migrmod)$tTable[3], scale1 = 5,
                       #           scale2 = 10))
summary(ranef4.Migrmod)

coeffs<-coef(MigrModC)
write.csv(coeffs,paste0("NSD/Coef_all_1perDay_NSD.csv"))

#################################################
# 1 per day NSD, only MigC  ----------------
#################################################
prongs.all <- read.csv("data/DataForAnalysis/ALL_1perDayNSD_30dMSD.csv")

good.id.migc <- as.character(prongs.class[prongs.class$bestMod.CC1ID == "MigrModC",]$id)
good.id <- as.character(good.id.migc)

prongs.mig <- data.frame()
for (i in unique(prongs.all$id)) {
  if (i %in% good.id) {
    prongs.mig <- rbind(prongs.mig, prongs.all[prongs.all$id == i, ])
  }
}

prongs.df <- prongs.mig

mix.Migrmod <- nlme(NSD ~ asymA/(1+exp((xmidA-nDaysYr)/scale1)) + (-asymB /(1 + exp((xmidB-nDaysYr)/scale2))),
                    data=prongs.df,
                    fixed = asymA + asymB + xmidA + xmidB + scale1 + scale2 ~ 1,
                    random = asymA + asymB +xmidA + xmidB~ 1,
                    groups =~ id,
                    start = c(asymA = 10000, asymB = 8000, xmidA = 100, xmidB = 300, scale1 = 10, scale2 = 8))
# start = c(asymA = 10000, asymB = 8000, xmidA = 110, xmidB = 300, scale1 = 8, scale2 = 12))
summary(mix.Migrmod)

coeffs<-coef(MigrModC)
write.csv(coeffs,paste0("NSD/Coef_MigC_only_1perDay_NSD.csv"))

#################################################
# original res NSD, all MigC and Mig, let's do seperate by year (each year has certain inerval) ----------------
#################################################
prongs.all <- read.csv ("data/DataForAnalysis/PAPO_20_NSD.csv")   #change this 
#prongs.all$id <- paste0("PAPO_", prongs.all$id)
prongs.all$date <- as.POSIXct(strptime(as.character(prongs.all$date), "%Y-%m-%d %H:%M",tz = "MST"))

prongs.mig <- data.frame()
for (i in unique(prongs.all$id)) {
  if (i %in% good.id) {
    prongs.mig <- rbind(prongs.mig, prongs.all[prongs.all$id == i, ])
  }
}

#check year. if in the same year, keep going next step
year <- vector()
for (i in unique(prongs.mig$id)) {
  year <- c(year, prongs.info[prongs.info$Location.ID == i, ]$ef.year)
}
unique(year)  # if more than 1, continue next block. Otherwise skip to prongs.df <- prongs.mig

##
prongs.mig.1 <- data.frame()
for (i in unique(prongs.mig$id)) {
  if (prongs.info[as.character(prongs.info$Location.ID) == i, ]$ef.year == unique(year)[2] ) {  #change year
    prongs.mig.1 <- rbind(prongs.mig.1, prongs.mig[prongs.mig$id == i, ])
  }
  else {next}
}
year <- vector()
for (i in unique(prongs.mig.1$id)) {
  year <- c(year, prongs.info[prongs.info$Location.ID == i, ]$ef.year)
}
unique(year)
prongs.mig <- prongs.mig.1
##

prongs.df <- prongs.mig[!is.na(prongs.mig$NSD), ]

mix.Migrmod <- nlme(NSD ~ asymA/(1+exp((xmidA-nDaysYr)/scale1)) + (-asymB /(1 + exp((xmidB-nDaysYr)/scale2))),
                    data=prongs.df,
                    fixed = asymA + asymB + xmidA + xmidB + scale1 + scale2 ~ 1,
                    random = asymA + asymB +xmidA + xmidB~ 1,
                    groups =~ id,
                    start = c(asymA = 14000, asymB = 10000, xmidA = 140, xmidB = 320, scale1 = 20, scale2 = 10))
# start = c(asymA = 10000, asymB = 8000, xmidA = 110, xmidB = 300, scale1 = 8, scale2 = 12))
summary(mix.Migrmod)

coeffs<-coef(mix.Migrmod)
write.csv(coeffs,paste0("NSD/Coef_MigConly_yr2016_OrgRes_NSD.csv"))  #change!!!!!
