#Step 6: plot migration parameters (distance and timing)

# Created: Jan 2019

# color code: 
# YEAR:
# 2010 "#e5cf6c"; 2011 "#9dbe59"; 2012 "#5bbe94"; 2013 "#5884b3"; 2014 "#cc6686"; 2016 "#e68570"
# Location:
# Mesa f8766d; Ref: 7cae00; Rock Springs 00bfc4; Sand Draw c77cff
# Season:
# Spring mig:a5da9b; Fall Mig: daa29c

# input: analysis/NSD/Coef_MigConly_yr201x_OrgRes_NSD.csv, data/dataforanalysis/FINAL_ALLanimalTableWX
# output: 

library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(ggExtra)
library(RColorBrewer) 

setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/")
prongs.info <- read.csv("data/DataForAnalysis/FINAL_ALLanimalTableWX.csv")

# NSD.2010 <- read.csv("NSD/Coef_MigConly_yr2010_OrgRes_NSD.csv")
# NSD.2011 <- read.csv("NSD/Coef_MigConly_yr2011_OrgRes_NSD.csv")
# NSD.2012 <- read.csv("NSD/Coef_MigConly_yr2012_OrgRes_NSD.csv")
# NSD.2013 <- read.csv("NSD/Coef_MigConly_yr2013_OrgRes_NSD.csv")
# NSD.2014 <- read.csv("NSD/Coef_MigConly_yr2014_OrgRes_NSD.csv")
# NSD.2016 <- read.csv("NSD/Coef_MigConly_yr2016_OrgRes_NSD.csv")
# NSD <- rbind(NSD.2016,
#              rbind(NSD.2014,
#                    rbind(NSD.2013,
#                          rbind(NSD.2012, 
#                                rbind(NSD.2010, NSD.2011)))))
# 
# colnames(NSD) <- c("Location.ID","asymA","asymB","xmidA","xmidB","scale1","scale2")
# 
# prongs.NSD <- left_join(prongs.info, NSD, by = "Location.ID")
# summary (prongs.NSD)
# 

# prongs.NSD <- data.frame(Location.ID = prongs.NSD$Location.ID, ef.mig = prongs.NSD$ef.mig, ef.year = prongs.NSD$ef.year,
#                          Capture.Location  = prongs.NSD$Capture.Area, 
#                          Mig.DistA  = prongs.NSD$asymA, Mig.DistB = prongs.NSD$asymB, 
#                          Mig.timingA  = prongs.NSD$xmidA , Mig.timingB = prongs.NSD$xmidB)  #the time number is number of days since 2.1 of each year 

# prongs.NSD <- prongs.NSD[!is.na(prongs.NSD$scale2),]
# for ( i in (1:nrow(prongs.NSD))) {
#   if (prongs.NSD$Mig.timingB[i] < prongs.NSD$Mig.timingA[i]) {
#     timeA <- prongs.NSD$Mig.timingB [i]
#     prongs.NSD$Mig.timingB[i] <- prongs.NSD$Mig.timingA[i]
#     prongs.NSD$Mig.timingA[i] <- timeA
#   }
# }

#write.csv(prongs.NSD, "data/DataForAnalysis/FINAL_ALLanimalTableWX_wNSD.csv")
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

# orginal data info plot ------------------
prongs.info$Capture.Area <- as.character(prongs.info$Capture.Area)
prongs.info <- prongs.info[!is.na(prongs.info$ef.year), ]
prongs.info$ef.year <- as.factor(prongs.info$ef.year)
for (i in 1:nrow(prongs.info)) {
  if (prongs.info$Capture.Area[i] == "" ) {
    prongs.info$Capture.Area[i] <- NA
  }
}


plot00 <- ggplot(prongs.info, aes(Capture.Area)) + geom_bar(aes(fill = ef.year))
Plot00 <- plot00 + 
  ylab("Invidual-year count") +
  theme_light() + 
  scale_fill_brewer(palette="Paired") +
  theme(legend.position = c(0.2, 0.7),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(1, "cm"),
        panel.border = element_blank(), 
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_blank())
Plot00

prongs.info$Capture.Area <- as.character(prongs.info$Capture.Area)
for (i in 1:nrow(prongs.info)) {
  if (startsWith(as.character(prongs.info$Location.ID[i]), "JMH")) {
    prongs.info$Capture.Area[i] <- "Rock Springs"
  }
}


plot00v2 <- ggplot(prongs.info, aes(Capture.Area)) + geom_bar(aes(fill = ef.year))
Plot00v2 <- plot00v2 + 
  ylab("Invidual-year count") +
  theme_light() + 
  scale_fill_brewer(palette="Paired") +
  theme(legend.position = c(0.9, 0.7),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(1, "cm"),
        panel.border = element_blank(), 
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_blank())
Plot00v2

prongs.info.1 <- prongs.info
prongs.info.1$Capture.Area <- as.character(prongs.info.1$Capture.Area)
for (i in 1:nrow(prongs.info.1)) {
  if (startsWith(as.character(prongs.info.1$Location.ID[i]), "JMH")) {
    prongs.info.1$Capture.Area[i] <- "Rock Springs"
  }
}

plot001 <- ggplot(prongs.info.1, aes(Capture.Area)) + geom_bar(aes(fill = ef.mig))
Plot001 <- plot001 + 
  ylab("Invidual-year count") +
  theme_light() + 
  scale_fill_brewer(palette="YlGnBu") +
  theme(legend.position = c(0.7, 0.7),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(1, "cm"),
        panel.border = element_blank(), 
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_blank())
Plot001


for (i in 1:nrow(prongs.info)) {
  if (startsWith(as.character(prongs.info$Location.ID[i]), "JMH")) {
    prongs.info$Capture.Area[i] <- "Rock Springs"
  }
}



# some of the data are not used in the nsd parameter analysis. here are the ones that are used
# stacked bar graph for used animal data info ------------------

plot0 <- ggplot(data, aes(Capture.Location)) + geom_bar(aes(fill = ef.year))
Plot0 <- plot0 + 
  ylab("Invidual-year count") +
  theme_light() + 
  scale_fill_brewer(palette="Paired") +
  theme(legend.position = c(0.9, 0.7),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(1, "cm"),
        panel.border = element_blank(), 
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_blank())
Plot0

plot01 <- ggplot(data, aes(Capture.Location)) + geom_bar(aes(fill = ef.mig))
Plot01 <- plot01 + 
  ylab("Invidual-year count") +
  theme_light() + 
  scale_fill_manual(values = c("#e9f9a2", "#246aa9")) + 
  theme(legend.position = c(0.9, 0.7),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(1, "cm"),
        panel.border = element_blank(), 
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_blank())
Plot01

plot02 <- ggplot(data, aes(ef.year)) + geom_bar(aes(fill = ef.mig))
Plot02 <- plot02 + 
  ylab("Invidual-year count") +
  theme_light() + 
  scale_fill_manual(values = c("#e9f9a2", "#246aa9")) + 
  theme(legend.position = c(0.9, 0.7),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(1, "cm"),
        panel.border = element_blank(), 
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_blank())
Plot02

# cannot be mapped - too few replicates for classes other than MigMix
# # bar graph for migration mode classification 
# prongs.class <- read.csv("NSD/ALL_All fMSD30_ClassificationOutput.csv")
# colnames(prongs.class)[14] <- "Location.ID"
# prongs.class <- left_join(prongs.info, prongs.class, by = "Location.ID")
# prongs.class <- prongs.class[!is.na(prongs.class$bestMod.CC1ID), ]
# prongs.class$ef.year <- as.character(prongs.class$ef.year)
# prongs.class$bestMod.CC1ID <- as.factor(prongs.class$bestMod.CC1ID)
# 
# plot03 <- ggplot(prongs.class, aes(ef.year)) + geom_bar(aes(fill = bestMod.CC1ID))
# Plot03 <- plot03 + 
#   ylab("Invidual-year count") +
#   theme_light() + 
#   scale_fill_brewer(palette="Paired") + 
#   theme(legend.position = c(0.9, 0.7),
#         legend.box.background = element_rect(colour = "black"),
#         legend.text = element_text(size = 10),
#         legend.title = element_blank(),
#         legend.key.size = unit(1, "cm"),
#         panel.border = element_blank(), 
#         axis.text.y = element_text(size = 15),
#         axis.title.y = element_text(size = 15),
#         axis.text.x = element_text(size = 15),
#         axis.title.x = element_blank())
# Plot03

# lollipop graph for migration timing --------------------------------------
plot1 <- ggplot(data) +
  geom_segment( aes(x=Location.ID, xend=Location.ID, y=Mig.seasonA, yend=Mig.seasonB), color= "grey")
Plot1 <- plot1 +
  geom_point( aes(x=Location.ID, y=Mig.seasonA), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=Location.ID, y=Mig.seasonB), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  scale_y_date(labels = date_format("%b")) +
  coord_flip() 
Plot1

# With a bit more style
Plot1 +
  theme_light() +
  theme(legend.position = "none",
        panel.border = element_blank(), 
        axis.text.y = element_text(size = 6)) +
  xlab("") +
  ylab("Date")


# violin plot for mig timing by Cap Loc -------------------------------
data$Capture.Location <- as.character(data$Capture.Location)

Plot1.2 <- ggplot(data, aes(factor(Capture.Location), Mig.seasonA)) + 
  geom_violin(aes(fill = as.factor(Capture.Location))) +
  ylab("Spring Migration Timing") +
  theme_light() + 
#  scale_fill_manual(values = c("#e5cf6c", "#9dbe59", "#5bbe94", "#5884b3", "#cc6686", "#e68570")) + 
  theme(legend.position = "none",
        panel.border = element_blank(), 
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_blank())
Plot1.2

Plot1.3 <- ggplot(data, aes(factor(Capture.Location), Mig.seasonB)) + 
  geom_violin(aes(fill = as.factor(Capture.Location))) +
  ylab("Fall Migration Timing") +
  theme_light() + 
  #  scale_fill_manual(values = c("#e5cf6c", "#9dbe59", "#5bbe94", "#5884b3", "#cc6686", "#e68570")) + 
  theme(legend.position = "none",
        panel.border = element_blank(), 
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_blank())
Plot1.3


# violin plot for mig timing by year ------------

Plot1.4 <- ggplot(data, aes(factor(ef.year), Mig.seasonA)) + 
  geom_violin(aes(fill = as.factor(ef.year))) +
  ylab("Spring Migration Timing") +
  theme_light() + 
  #  scale_fill_manual(values = c("#e5cf6c", "#9dbe59", "#5bbe94", "#5884b3", "#cc6686", "#e68570")) + 
  theme(legend.position = "none",
        panel.border = element_blank(), 
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_blank())
Plot1.4

Plot1.5 <- ggplot(data, aes(factor(ef.year), Mig.seasonB)) + 
  geom_violin(aes(fill = as.factor(ef.year))) +
  ylab("Fall Migration Timing") +
  theme_light() + 
  #  scale_fill_manual(values = c("#e5cf6c", "#9dbe59", "#5bbe94", "#5884b3", "#cc6686", "#e68570")) + 
  theme(legend.position = "none",
        panel.border = element_blank(), 
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_blank())
Plot1.5


# violin plot for migration distance - by capture location -------------
data.dist <- data.frame(Location.ID = data$Location.ID, Mig.distA = data$Mig.DistA, Mig.distB = data$Mig.DistB, ef.year = data$ef.year, Capture.Location = data$Capture.Location)
data.dist$Mig.dist <- sqrt(abs(data.dist$Mig.distA))
data.dist$Capture.Location <- as.character(data.dist$Capture.Location)
for (i in 1:nrow(data.dist)) {
  if (startsWith(as.character(data.dist$Location.ID[i]), "JMH")) {
    data.dist$Capture.Location[i] <- "Rock Springs"
  }
}

data.dist <- data.dist[!is.na(data$Capture.Location),]

# for (i in 1:nrow(data.dist)) {
#   if (data.dist[i, "Capture.Location"] == "Mesa") {
#     data.dist$mycolor[i] <- "#f8766d"
#   }
#   if (data.dist[i, "Capture.Location"] == "Ref") {
#     data.dist$mycolor[i] <- "#7cae00"
#   }
#   if (data.dist[i, "Capture.Location"] == "Rock Springs") {
#     data.dist$mycolor[i] <-"#00bfc4"
#   }
#   if (data.dist[i, "Capture.Location"] == "Sand Draw") {
#     data.dist$mycolor[i] <- "#c77cff"
#   }
# }

# First type of color
Plot3 <- ggplot(data.dist, aes(factor(Capture.Location), Mig.dist)) + 
  geom_violin(aes(fill = Capture.Location)) +
  ylab("Round-trip Migration Distance (km)") +
  theme_light() + 
  theme(legend.position = "none",
        panel.border = element_blank(), 
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_blank())
Plot3

# violin plot for migration distance - by year -------------
Plot4 <- ggplot(data.dist, aes(factor(ef.year), Mig.dist)) + 
  geom_violin(aes(fill = as.factor(ef.year))) +
  ylab("Migration Distance (km)") +
  theme_light() + 
  scale_fill_manual(values = c("#e5cf6c", "#9dbe59", "#5bbe94", "#5884b3", "#cc6686", "#e68570")) + 
  theme(legend.position = "none",
        panel.border = element_blank(), 
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_blank())
Plot4

# lollipop ordered by spring distance  ----------------


data.dist.1 <- data.dist %>% rowwise() %>% arrange(Mig.dist) %>% mutate(Location.ID=factor(Location.ID, Location.ID))

plot5 <- ggplot(data.dist.1, aes(x = Location.ID, y = Mig.dist)) +
  geom_segment( aes(x=Location.ID, xend=Location.ID, y=0, yend=Mig.dist, color = Capture.Location), size=1.3, alpha=0.9)
Plot5 <- plot5 +
  theme_light() +
  scale_color_discrete(labels = c("Mesa", "Ref", "Rock Springs", "Sand Draw")) +
  theme(legend.position = c(0.2, 0.7),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.size = unit(1, "cm"),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16, margin = margin (t =0, r = 20, b=0, l=0))) +
  labs(x = " ", y = "Round trip migration distance (km)", color = "Capture Location\n")
Plot5


# lollipop ordered by the max NSD value -----------------------
data.NSD$id <- as.factor(data.NSD$id)
colnames(data.dist)[1] <- "id"
MaxNSD <- data.NSD %>% group_by(id) %>% summarise(MaxNSD = max(NSD))
data.dist <- left_join(data.dist, MaxNSD, by = "id")

data.dist.2 <- data.dist %>% rowwise() %>% arrange(MaxNSD) %>% mutate(Location.ID=factor(id, id))

plot5.5 <- ggplot(data.dist.2, aes(x = id, y = sqrt(MaxNSD))) +
  geom_segment( aes(x=Location.ID, xend=Location.ID, y=0, yend= sqrt(MaxNSD), color = Capture.Location), size=1.3, alpha=0.9)
Plot5.5 <- plot5.5 +
  theme_light() +
  scale_color_discrete(labels = c("Mesa", "Ref", "Rock Springs", "Sand Draw")) +
  theme(legend.position = c(0.2, 0.7),
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.size = unit(1, "cm"),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16, margin = margin (t =0, r = 20, b=0, l=0))) +
  labs(x = " ", y = "Max SQRT(NSD) (km)", color = "Capture Location\n")
Plot5.5
 
###########################################
# # # # # # plot NSD --------------------
###########################################
data.NSD <- read.csv ("data/DataForAnalysis/ALL_1perDayNSD_30dMSD.csv")
data.NSD$Date <- as.POSIXct(strptime(as.character(data.NSD$Date), format = "%Y-%m-%d"))

#  NSD of the ones that MigB <0 ------------
id.list <- data.dist[data.dist$Mig.distB < 0, ]$Location.ID
data.NSD$Date <- as.Date(data.NSD$date)
plot6<-qplot(Date, sqrt(NSD), data = data.NSD[data.NSD$id %in% id.list, ], geom = "path", colour = id)

Plot6 <- plot6 +  geom_path(size = 0.7) +
  theme(axis.title.y = element_text(size = 12, vjust = 0.3, face = "bold")) +
  theme(axis.title.x = element_text(size = 12, vjust = 0.5, face = "bold")) +
  scale_x_date(labels = date_format("%b")) +
  theme(panel.border = element_rect(colour = "black", fill = "NA")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor.x = element_line(colour="white")) +
  theme(panel.grid.minor.y = element_line(colour="white")) + xlab("Date") + ylab("Distance (km)") +
  theme(axis.text.y = element_text(face = "bold", size = rel(1.3))) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(face = "bold", size = rel(1.3)))
# by id, all individuals together
# Plot6
# by id, for each individual
Plot6 + facet_wrap(~id, scales = "free")

# NSD of the suffcient individuals -----------
id.list <- prongs.info[prongs.info$ef.mig == "Sufficient", ]$Location.ID
data.NSD$Date <- as.Date(data.NSD$date)
plot6.05<-qplot(Date, sqrt(NSD), data = data.NSD[data.NSD$id %in% id.list, ], geom = "path", colour = id)

Plot6.05 <- plot6.05 +  geom_path(size = 0.7) +
  theme(axis.title.y = element_text(size = 12, vjust = 0.3, face = "bold")) +
  theme(axis.title.x = element_text(size = 12, vjust = 0.5, face = "bold")) +
  scale_x_date(labels = date_format("%b")) +
  theme(panel.border = element_rect(colour = "black", fill = "NA")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor.x = element_line(colour="white")) +
  theme(panel.grid.minor.y = element_line(colour="white")) + xlab("Date") + ylab("Distance (km)") +
  theme(axis.text.y = element_text(face = "bold", size = rel(1.3))) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(face = "bold", size = rel(1.3)))
Plot6.05 + facet_wrap(~id, scales = "free")

#  NSD of the ones in 2011 ------------
id.list <- data.dist[(data.dist$ef.year == "2011"), ]$Location.ID
data.NSD$Date <- as.Date(data.NSD$date)
plot6.11 <-qplot(Date, sqrt(NSD), data = data.NSD[data.NSD$id %in% id.list, ], geom = "path", colour = id)

Plot6.11 <- plot6.11 +  geom_path(size = 0.7) +
  theme(axis.title.y = element_text(size = 12, vjust = 0.3, face = "bold")) +
  theme(axis.title.x = element_text(size = 12, vjust = 0.5, face = "bold")) +
  scale_x_date(labels = date_format("%b")) +
  theme(panel.border = element_rect(colour = "black", fill = "NA")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor.x = element_line(colour="white")) +
  theme(panel.grid.minor.y = element_line(colour="white")) + xlab("Date") + ylab("Distance (km)") +
  theme(axis.text.y = element_text(face = "bold", size = rel(1.3))) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(face = "bold", size = rel(1.3)))
# by id, all individuals together
# Plot6
# by id, for each individual
Plot6.11 + facet_wrap(~id, scales = "free")

#  NSD of the ones in 2014 ------------
id.list <- data.dist[(data.dist$ef.year == "2014"), ]$Location.ID
data.NSD$Date <- as.Date(data.NSD$date)
plot6.14 <-qplot(Date, sqrt(NSD), data = data.NSD[data.NSD$id %in% id.list, ], geom = "path", colour = id)

Plot6.14 <- plot6.14 +  geom_path(size = 0.7) +
  theme(axis.title.y = element_text(size = 12, vjust = 0.3, face = "bold")) +
  theme(axis.title.x = element_text(size = 12, vjust = 0.5, face = "bold")) +
  scale_x_date(labels = date_format("%b")) +
  theme(panel.border = element_rect(colour = "black", fill = "NA")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor.x = element_line(colour="white")) +
  theme(panel.grid.minor.y = element_line(colour="white")) + xlab("Date") + ylab("Distance (km)") +
  theme(axis.text.y = element_text(face = "bold", size = rel(1.3))) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(face = "bold", size = rel(1.3)))
# by id, all individuals together
# Plot6
# by id, for each individual
Plot6.14 + facet_wrap(~id, scales = "free")

# MSD for everyone -----------
data.NSD$Date <- as.Date(data.NSD$date)
plot6.15 <-qplot(Date, sqrt(MSD), data = data.NSD, geom = "path", colour = id)

Plot6.15 <- plot6.15 +  geom_path(size = 0.7) + 
  theme(axis.title.y = element_text(size = 12, vjust = 0.3, face = "bold")) +
  theme(axis.title.x = element_blank()) +
  scale_x_date(labels = date_format("%Y")) +
  theme(panel.border = element_rect(colour = "black", fill = "NA")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor.x = element_line(colour="white")) +
  theme(panel.grid.minor.y = element_line(colour="white")) + xlab("Date") + ylab("Distance (km)") +
  theme(axis.text.y = element_text(face = "bold", size = rel(1.3))) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(face = "bold", size = rel(1.3)))
# by id, all individuals together
Plot6.15
# by id, for each individual
# Plot6 + facet_wrap(~id, scales = "free")

# MSD classification result plot ----------------
data.class <- read.csv("NSD/ALL_All MSD30_ClassificationOutput.csv")
plot7 <- ggplot(data.class, aes(x = factor(1), fill = bestMod.CC1ID)) + geom_bar(width = 1)
Plot7 <- plot7 + 
  ylab("Count") +
  theme_light() + 
  scale_fill_brewer(palette="Paired") +
  theme(
#        legend.position = c(0.9, 0.7),
#        legend.box.background = element_rect(colour = "black"),
#        legend.text = element_text(size = 10),
#        legend.title = element_blank(),
        legend.key.size = unit(1, "cm"),
        panel.border = element_blank(), 
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_blank())
Plot7
