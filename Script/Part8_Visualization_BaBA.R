# this is to make graphs to visualize results and sensitivity analysis 
library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)
library(viridis)
library(OneR)
library(binr)
library(foreach)
#library(plotly)

######### straightness v.s. distance to fence ###########
# the data is only for migration season (Apr - Oct)
strdist <- read.csv("C:/Users/wenjing.xu/Google Drive/RESEARCH/Pronghorn/Analysis/FenceBehavior/DataTable/PP_Official/I2_PP10_Dist2Fence.csv")
#strdist <- read.csv("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/FenceBehavior/DataTable/DistanceAnalysis/I2_4to48_10ind.csv")
strdist$duration <- as.character(strdist$duration)
strdist <- strdist %>% filter ((straightness > 0) & (straightness < 1))
str.dist.612 <- strdist %>% filter (duration == "12" | duration == "6")

# ------ scatter plot: dist2fence vs straightness ----------------
plot.strdist <- ggplot(str.dist.612, aes(dist2fence, y = straightness)) +
  geom_point(aes(color = duration), size = 1) + 
  labs ( x = "Distance to fence (m)", y = "movement segment straightness") +
  theme_bw()
plot.strdist

# ----- box plot, sd of straightness VS distance to fence -----
strdist$dist2fence.bin <- bin(strdist$dist2fence, nbin = 4)
strdist.var <- strdist %>% filter(duration == 12) %>% dplyr::group_by(dist2fence.bin)  %>% dplyr::summarise(str.sd = sd(straightness))
#strdist.var <- strdist %>% dplyr::group_by(dist2fence.bin)  %>% dplyr::summarise(str.sd = sd(straightness))
strdist.var$minend <-  gsub("\\(", "", strdist.var$dist2fence.bin)
strdist.var$minend <-  gsub("\\]", "", strdist.var$minend)
strdist.var$minend <- gsub("\\,.*", "", strdist.var$minend)
strdist.var[strdist.var$minend == -11.2,]$minend <- 0

a <- bins.quantiles(strdist$dist2fence,  target.bins = 4, max.breaks = 4)


plot.sd.dist2fence <- ggplot(data = strdist.var, aes(x = minend, y = str.sd)) +
  geom_col() +
  labs ( x = "distance to fences", y = "standard deviation of straightness")
plot.sd.dist2fence

plot.sd.dist2fence2 <- ggplot(data = strdist, aes(x = dist2fence.bin, y = straightness)) +
  geom_boxplot() +
  labs ( x = "distance to fences", y = "straightness")
plot.sd.dist2fence2


# ----- box plot, speed VS distance to fence -----
dist2fence <- read.csv(paste0(getwd(),"/MidProcess/DistanceTable__MULE.txt"))
dist2fence <- dist2fence %>% filter(!is.na(Easting))

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

x1 <- cbind(dist2fence$Easting, dist2fence$Northing)

a <- foreach(i = 1:(nrow(x1)-1), .combine = c ) %do% euc.dist(x1[i,],x1[i+1,])

dist2fence$speed <- c(NA, a/2)
dist2fence <- dist2fence[2:nrow(dist2fence),]

dist2fence$dist2fence.bin <- bin(dist2fence$NEAR_DIST, nbin = 8)
#strdist.6 <-strdist.6 %>% filter(speed <1500)
#strdist.speed.var <- strdist.6  %>% dplyr::group_by(speed.bin)  %>% dplyr::summarise(speed.sd = sd(straightness))
#strdist.var <- strdist %>% dplyr::group_by(dist2fence.bin)  %>% dplyr::summarise(str.sd = sd(straightness))
strdist.speed$minend <-  gsub("\\(", "", strdist.speed$dist2fence.bin)
strdist.speed$minend <-  gsub("\\]", "", strdist.speed$minend)
strdist.speed$minend <- gsub("\\,.*", "", strdist.speed$minend)
strdist.speed[strdist.speed$minend == -9.24,]$minend <- 0

plot.sd.dist2fence <- ggplot(data = strdist.speed , aes(x = dist2fence.bin, y = speed)) +
  geom_boxplot() +
  labs ( x = "distance to fences", y = "movement speed")
plot.sd.dist2fence



# ------ density plot for distrance: all duration ---------------
color = c("#996888", "#C99DA3", "#C6DDF0", "#ffffff")
rects <- data.frame(xstart = c(summary(strdist$dist2fence)[1], summary(strdist$dist2fence)[2], summary(strdist$dist2fence)[3], summary(strdist$dist2fence)[5]), 
                    xend = c(summary(strdist$dist2fence)[2], summary(strdist$dist2fence)[3], summary(strdist$dist2fence)[5], summary(strdist$dist2fence)[6]))
ymax = max(density(strdist$dist2fence)$y)

plot.distden <- ggplot() +
  geom_density(data = strdist, aes(x = dist2fence, color = duration), size = 1) + 
  labs ( x = "Distance to fence (m)", y = "Density") +
  geom_rect(data=rects, aes(ymin=0, ymax= ymax+0.00005, xmin=xstart,
                            xmax=xend), fill = color, alpha =0.4) +
  theme_bw()
plot.distden + 
  geom_text(aes(x = summary(strdist$dist2fence)[2], y = 0.0003, 
                             label = "1st qu. = 215.2945 m"), angle=90, vjust = 1.2, size = 4) +
  geom_text(aes(x = summary(strdist$dist2fence)[3], y = 0.0003, 
                label = "median = 422.6545 m"), angle=90, vjust = 1.2, size = 4) +
  geom_text(aes(x = summary(strdist$dist2fence)[5], y = 0.0003, 
                label = "3rd qu. = 947.2226 m"), angle=90, vjust = 1.2, size = 4) 

summary(strdist$dist2fence)

# ------ density plot for straightness: all duration ---------------
str <- read.csv("C:/Users/wenjing.xu/Google Drive/RESEARCH/Pronghorn/Analysis/FenceBehavior/DataTable/Straightness/Straightness_Lookup_10ind.csv")
#str <- read.csv("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/FenceBehavior/DataTable/Straightness/Straightness_Lookup_10ind.csv")

str$duration <- as.factor(str$duration)

plot.strden <- ggplot(str, aes(x = straightness)) +
  geom_density(aes(color = duration), size = 1) + 
  labs ( x = "Straightness", y = "Density") +
theme_bw()
plot.strden
# when the time window is 18h, the distribution of straightness during migration season is relatively non-skewed

# ------ density plot for dist2fence: duration 6, with mean and peak ---------------
str.dist.6 <- strdist %>% filter (duration == "6")
qu1 <-as.numeric(summary(str.dist.6$dist2fence)[2])
qu2 <-as.numeric(summary(str.dist.6$dist2fence)[3])

#str.dist.den <-  density(str.dist.6$dist2fence)
#max.loc <- str.dist.den$x[which.max(str.dist.den$y)]
#peak <- data.frame(duration = 6, x = str.dist.den$x[which.max(str.dist.den$y)])
#grp.mean = mean(str.dist.6$dist2fence)

plot.distden.6 <- ggplot(str.dist.6, aes(x = dist2fence)) +
  geom_density(color = '#EA992E', size = 1.5) + 
  xlim(0, 6000) +
  geom_vline(xintercept = qu1, color = '#ea4d2e', size = 1) +
  geom_vline(aes(xintercept= qu2, color="ea4d2e"),linetype="dashed", size = 1) +
  labs (title = "Distribution of distances from pronghorn locations during migration to fences (smoothed)", x = "Density", y = "Distance to Fences (m)") +
  theme_bw() + 
  theme(legend.position = "none") 
plot.distden.6

plot.distden.6 + 
  geom_text(aes(x = qu1, y = 0.0003, 
                           label = "1st Quantile = 272.67 m"), angle=90, vjust = 1.3, size = 4) +
  geom_text(aes(x = qu2, y = 0.0003, 
                             label = "2nd Quantile = 604.91 m"), angle=90, vjust = 1.3, size = 4) 

  
# ------ box/violin plot for dist2fence ---------------
strdist %>% ggplot(aes(x = duration, y = dist2fence, fill = duration)) +
  geom_boxplot(width = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  labs ( x = "Movement segment duration", y = "Distance to fence (m)")

sample_size = strdist %>% group_by(duration) %>% dplyr::summarize(num=n())

strdist %>% left_join(sample_size) %>% 
  dplyr::mutate(myaxis = paste0(duration, "\n", "n = ", num)) %>%
  ggplot(aes(x = myaxis, y = dist2fence, fill = duration)) +
  geom_violin(width = 1) +
  geom_boxplot(width = 0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  labs ( x = "Movement segment duration", y = "Distance to fence (m)")




######### line plot: straightness v.s. window size  ################
#straightness <- read.csv("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/FenceBehavior/DataTable/straightness_roll_5step.csv")
straightness <- read.csv("C:/Users/wenjing.xu/Google Drive/RESEARCH/Pronghorn/Analysis/FenceBehavior/DataTable/straightness_roll_5step.csv")
straightness <- straightness[!is.na(straightness$Straightness), ]
straightness$AnimalID <- as.factor(straightness$AnimalID)
interval <- 2
straightness$window.size <- straightness$window.size*interval

# window size * straightness, population average ------------
straightness.pop <- straightness %>% 
                  group_by(window.size) %>% 
                  summarize( sd = sd (Straightness), 
                             mean = mean (Straightness))
straightness.pop <- straightness.pop %>% 
                  group_by(window.size) %>% 
                  mutate (low = (mean - (1/2)*sd) , 
                             high = (mean + (1/2)*sd))
straightness.pop <- as.data.frame(straightness.pop)

plot.pop <- ggplot(straightness.pop, aes(x = window.size)) + 
  geom_line(aes(y = mean)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) 
plot.pop

# window size * straightness, color by individual ------------
straightness.ind <- straightness %>% 
  group_by(window.size, AnimalID) %>% 
  summarize( sd = sd (Straightness), 
             mean = mean (Straightness))
straightness.ind <- straightness.ind %>% 
  group_by(window.size) %>% 
  mutate (low = (mean - (1/2)*sd) , 
          high = (mean + (1/2)*sd))
straightness.ind <- as.data.frame(straightness.ind)

plot.ind <- ggplot(straightness.ind, aes(window.size, y = mean)) +
  geom_ribbon(aes(ymin = low, ymax = high, fill = AnimalID), alpha = 0.1) +
  geom_line(aes(color = AnimalID), size = 1) + 
  labs ( x = "window size (hours)", y = "movement segment straightness") +
  theme_bw() + 
  theme(legend.position = "none") 
plot.ind


# window size * straightness, color by month ------------
straightness$month <- strftime(as.POSIXct(strptime(straightness$Date, "%Y-%m-%d %H:%M:%S")), "%m")
straightness.mo <- straightness %>% 
  group_by(window.size, month) %>% 
  summarize( sd = sd (Straightness), 
             mean = mean (Straightness))
straightness.mo <- straightness.mo %>% 
  group_by(window.size) %>% 
  mutate (low = (mean - (1/2)*sd) , 
          high = (mean + (1/2)*sd))

plot.mo <- ggplot(straightness.mo, aes(window.size, y = mean)) +
  geom_ribbon(aes(ymin = low, ymax = high, fill = month), alpha = 0.1) +
  geom_line(aes(color = month), size = 1) + 
  labs ( x = "window size (hours)", y = "movement segment straightness") +
  theme_bw() + 
  guides(fill=guide_legend(ncol=2)) + 
  theme(legend.position=c(.85, .8))
plot.mo

######### classification result plot ################
# w3 <- read.csv("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/FenceBehavior/DataTable/RESULTS/I2_FB500_B4_P48_W3.csv")
# w7 <- read.csv("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/FenceBehavior/DataTable/RESULTS/I2_FB500_B4_P48_W7.csv")
# w10 <- read.csv("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/FenceBehavior/DataTable/RESULTS/I2_FB500_B4_P48_W10.csv")
# w14 <- read.csv("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/FenceBehavior/DataTable/RESULTS/I2_FB500_B4_P48_W14.csv")
# w30 <- read.csv("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/FenceBehavior/DataTable/RESULTS/I2_FB500_B4_P48_W30.csv")

#result <-  read.csv("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/FenceBehavior/DataTable/RESULTS/I2all_FB250_B4_P48_W7_mig.csv")

#result <-  read.csv("C:/Users/wenjing.xu/Google Drive/RESEARCH/Pronghorn/Analysis/FenceBehavior/DataTable/RESULTS/I2new_FB250_B4_P48_W7_mig.csv")

#whole year result
#result <-  read.csv("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/FenceBehavior/DataTable/RESULTS/old10ind/I2_FB250_B4_P48_W7.csv")

# -----add lat long for kepler visualization ----
# types <- data.frame(AnimalID = w3$AnimalID, date = w3$burstID, easting = w3$easting, northing = w3$northing, straightness = w3$straightness, duration = w3$duration,
#                     w3_Type = w3$eventTYPE, w7_Type = w7$eventTYPE, w10_Type = w10$eventTYPE, w14_Type = w14$eventTYPE, w30_Type = w30$eventTYPE)

types <- data.frame(AnimalID = result$AnimalID, date = result$burstID, easting = result$easting, northing = result$northing, straightness = result$straightness, duration = result$duration,
                    result_Type = result$eventTYPE)
target.crs <- "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#xy <- cbind(types$easting, types$northing)
#type.sp <- SpatialPointsDataFrame (coords = xy, data = types, proj4string = CRS(target.crs))
#proj4string(type.sp)
type.ll<- types
coordinates(type.ll) <- ~easting + northing
proj4string(type.ll) <- CRS(target.crs)

type.ll<- spTransform(type.ll, CRS("+proj=longlat"))
type.ll<- data.frame(types, coordinates(type.ll))
type.ll <- type.ll[,c(1,2,3,4,8,9,5,6,7)]

type.ll$date <- as.POSIXct(strptime(as.character(type.ll$date),"%Y-%m-%d %H:%M"))  
type.ll$time <- strftime(type.ll$date,"%m/%d/%y %H:%M")

write.csv(type.ll, file = paste0( getwd(), "/DataTable/I2new_FB250_B4_P48_latlong.csv"))

# ---- month * types, colored by Type ---------
#types <- data.frame(AnimalID = w3$AnimalID, date = w3$burstID, easting = w3$easting, northing = w3$northing, straightness = w3$straightness, duration = w3$duration,
#                    w3 = w3$eventTYPE, w7 = w7$eventTYPE, w10 = w10$eventTYPE, w14 = w14$eventTYPE, w30 = w30$eventTYPE)
types <- data.frame(AnimalID = result$AnimalID, date = result$burstID, easting = result$easting, northing = result$northing, straightness = result$straightness, duration = result$duration,
                    result_Type = result$eventTYPE)
types$month <- strftime(as.POSIXct(strptime(types$date, "%Y-%m-%d %H:%M")), "%m")

#types.1 <- types %>% gather("window.size", "Type", 7:11)
unique(types$result_Type)
types.1 <- types %>% dplyr::group_by(AnimalID, month) %>% dplyr::summarise(Bounce = sum(result_Type == "Bounce"), 
                                                   Trapped = sum(result_Type == "Trapped"),
                                                   Back_n_forth = sum(result_Type == "Back-n-forth"),
                                                   Trace = sum(result_Type == "Trace"),
                                                   Average_Movement = sum(result_Type == "Average Movement"),
                                                   QuickCross = sum(result_Type == "Quick Cross"),
                                                   Unknown = sum(result_Type == "unknown"))


types.1 <- types.1 %>% dplyr::select(-Unknown)

types.1 <- types.1 %>% group_by(month) %>% tidyr::gather("Type", "Count", 3:8)
  
# plot.type.mo <- ggplot(data = types.1, aes(x = month, y = Count, group = Type)) +
#   geom_line(aes(color= Type), size = 1.5) + 
#   theme_bw() + 
#   #guides(fill=guide_legend(ncol=2)) + 
#   scale_colour_manual(values = c("#97A0A7","#A4243B", "#ea992e", "#5D6A75", "#4a7856", "#393E41")) + 
#   labs(title="Types of response to fences over time",x = "Month", y = "Frequency") +
#   theme(legend.position=c(.9, .8))
# plot.type.mo

# ----- lines (average occurrence across months) ------
types.2 <- types.1 %>% filter(AnimalID == 104) %>% group_by(Type, month) %>% dplyr::summarise(frequency = mean(Count))
types.21 <- types.2 %>% filter (Type == "Back_n_forth" | Type == "Bounce" | Type == "Trace")
types.22 <- types.2 %>% filter (Type != "Back_n_forth" & Type != "Bounce" & Type != "Trace")
types.21$lsize <- as.character(2)
types.22$lsize <- as.character(1)
types.2 <- rbind(types.21, types.22)

ggplot(types.2, aes(x=month, y=frequency, group=Type, color = Type)) + 
 # geom_errorbar(aes(ymin=frequency-sd, ymax=frequency+sd), width=.1) +
  geom_line(aes(size = lsize)) + geom_point(size = 3) +
  theme_bw() + 
  scale_colour_manual(values = c("#A3B18A","#FF6666", "#FFDC5E", "#BBCE65", "#FFA159", "#395E66")) +
  scale_fill_manual(values = c("#A3B18A","#FF6666", "#FFDC5E", "#BBCE65", "#FFA159", "#395E66")) + 
  scale_size_manual(values = c(0.5, 1.5)) + 
  theme(legend.position="none") + 
  #guides(color = guide_legend(override.aes = list(size=2.5))) + 
  #theme(legend.key.size = unit(1.3, "cm")) +
  #theme(legend.text=element_text(size=15)) + 
  labs(title="Fence encounter behaviors of one individual (PAPO_163) over one year", x = "Month", y = "Occurrence")


# ----- box plot for frequency by month by types -------

types11 <- types.1 %>% filter (Type == "Back_n_forth" | Type == "Bounce" | Type == "Trace")
types12 <- types.1 %>% filter (Type != "Back_n_forth" & Type != "Bounce" & Type != "Trace"  & Type != "Trapped")

ggplot(types11, aes(x=month, y=Count, fill=Type)) + 
  geom_boxplot() +
  theme_bw() + 
#  scale_colour_manual(values = c("#FF6666", "#FFDC5E", "#FFA159")) +
  scale_fill_manual(values = c("#FF6666", "#FFDC5E", "#FFA159")) + 
#  theme(legend.position="none") + 
  labs(title="Responsive behaviors when encounterinf fences", x = "Month", y = "Occurrence")

ggplot(types12, aes(x=month, y=Count, fill=Type)) + 
  geom_boxplot() +
  ylim(0, max(types11$Count)) +
  theme_bw() + 
  scale_fill_manual(values = c("#A3B18A", "#BBCE65")) +
#  theme(legend.position="none") + 
  labs(title="Normal movements when encountering fences", x = "Month", y = "Occurrence")



# ------- bar plot for each types ------------
types.11 <- types.1 %>% filter ((Type == "Average_Movement") | (Type == "QuickCross")) %>% mutate(Gen.Type = "Normal")
types.12 <- types.1 %>% filter ((Type == "Back_n_forth") | (Type == "Bounce") | (Type == "Trace")) %>% mutate(Gen.Type = "Responsive")
types.13 <- types.1 %>% filter (Type == "Trapped") %>% mutate(Gen.Type = "Trapped")

type.2 <- rbind(types.11, types.12, types.13)
type.2$Gen.Type <- factor(type.2$Gen.Type, levels = c("Normal", "Responsive", "Trapped"))

ggplot(data=type.2, aes(x=Gen.Type, y=Count/10, fill=Type)) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values = c("#A3B18A","#FF6666", "#FFDC5E", "#BBCE65", "#FFA159", "#395E66")) +
  #scale_color_manual(values = c("#89AF46","#FFCE1E", "#FF5B5B", "#B0CE2B", "#FF5728", "#395E66")) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(x = "Types of response to fences", y = "Frequency")
  


# # Use position=position_dodge()
# ggplot(data=type.2, aes(x=Gen.Type, y=Count, fill=Type)) +
#   geom_bar(stat="identity", position=position_dodge())

# -------- classification rate (window size * non-unknown rate, bar plot) --------
types.2 <- types.1 %>% filter(Type == "Unknown") %>% group_by(window.size) %>% summarise( sum (Count))
types.2$rate <- types.2$`sum(Count)`/nrow(types)
types.2$window.size <- substr(types.2$window.size, 2, nchar(types.2$window.size))
types.2$window.size <- factor(types.2$window.size, levels = c("3", "7", "10", "14", "30"))

plot.rate <- ggplot(types.2, aes (x = window.size, y = rate)) +
  geom_segment( aes(x=window.size, xend=window.size, y=0, yend=rate)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(title="'Unknown' rate for each rolling window size", x = "Window Size", y = "Unclassiffied rate")
plot.rate

# -------- except for unknown, stacked bar plot for types. window size * freq, colored by types --------
types.3 <- types %>%  gather("window.size", "Type", 7:11) %>% filter(Type == "Back-n-forth" | Type == "Trace" |  Type == "unknown")
types.3$window.size <- factor( types.3$window.size, levels = c("w3", "w7", "w10", "w14", "w30"))
types.3 <- types.3 %>% group_by(window.size) %>% summarise(Back_n_forth = sum(Type == "Back-n-forth"),
                                                Trace = sum(Type == "Trace"), 
                                                Unknown = sum(Type == "unknown"))
types.3 <- types.3 %>% gather("Types", "n", 2:4)

types.3$perc <- numeric(nrow(types.3))
for ( i in c("w3", "w7", "w10", "w14", "w30")) {
  sum <- as.numeric((types.3 %>% filter(window.size == i) %>% summarise( sum (n))))
  types.3[types.3$window.size == i, ]$perc <- (types.3[types.3$window.size == i, ]$n) / sum 
}

plot.win.sensi <- ggplot(types.3, aes(fill=Types, y=perc, x=window.size)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("#A4243B", "#e26d5c", "#F3DE8A")) +
  labs(title="Sensitivity analysis - window size",x = "Window Size", y = "Type Percentage") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  )  + 
  theme(legend.position="bottom")
plot.win.sensi
  
  

