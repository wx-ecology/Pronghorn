##### STEP 1 Pronghorn preprocessing
# including detecting irregular intervals and modify them into the expected intervals, 
# and inserting NA rows for missing data point,
# and summarize the data information by ID into the animal table.
# Created: FALL 2018

# Output: JMHlocationTableWX.csv, JMHanimalTableWX.csv, PAPOlocationTableWX.csv, PAPOanimalTableWX.csv

# set up ----------------------------------------------
library(rgdal)
library(adehabitatLT)
library(adehabitatHR)
library(gdata)
library(nlme)
library(dplyr)

setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis")

# PAPO dataset ------------------------------------------------------------
pinedale.raw <- read.csv("data/PAPOlocationTable.csv")
pinedale.raw$time <- paste0(pinedale.raw$Year,"-", pinedale.raw$Month,"-",pinedale.raw$"Day", " ", pinedale.raw$Hour, ":00")
#convert times from factors to POSIX
pinedale.raw$time <- as.POSIXct(strptime(as.character(pinedale.raw$time), "%Y-%m-%d %H:%M"))
# detect duplication ------------------------------------------------------
# sum(duplicated(paste(pinedale.raw$time, pinedale.raw$Location.ID)))
# #check where are the duplicates
# index <- which(duplicated(paste(pinedale.raw$time, pinedale.raw$Location.ID))==TRUE)
#there are 2 ID "75" but no "72". Change the first "75" to "72"
pinedale.raw[153405:154660,]$Location.ID <- rep(72, 154660-153405+1)

# intervals detection ---------------------------------------------------
pinedale.int <- select(pinedale.raw, Location.ID, Year, Month, Day, Hour, Latitude, Longitude, Easting, Northing, Altitute, Horizontal, time)
pinedale.int <- pinedale.int %>%
  group_by(Location.ID) %>% 
  mutate (interval = difftime(time, lag(time, default = 0)))
pinedale.int <- pinedale.int[duplicated(pinedale.int$Location.ID),] #delete the first row of each ID (time interval calculation for the first row is always weird)
# #find out since when the intervals become a regular 2. Second order differences
# pinedale.int <- pinedale.int %>%
#   group_by(Location.ID) %>%
#   mutate (interval.2 = interval - lag(interval, default = 0))
# pinedale.int <- pinedale.int[duplicated(pinedale.int$Location.ID),] #delete the first row of each ID again
## find out which antelopes have regular 2H intervals
# plot(pinedale.int$interval.2)
### So individual 1-30 - 3.5h (supposedly); 31-98 - 3h, 99-164 - 2h
## to visualize the intervals 
# plot(pinedale.int[pinedale.int$Location.ID > 98,]$interval.2)   #some indivudals have more missing data than others.


##################### for check back, starting here ##########   ---------------------------------------
## so group them into three different ones and add place holder if missing data
#pinedale.int35 <- pinedale.int[pinedale.int$Location.ID <31, ]
#write.csv(pinedale.int35, "data/pinedale.int35.raw.csv")
pinedale.int35 <- read.csv("data/pinedale.int35.raw.csv")
#pinedale.int3 <- pinedale.int[(pinedale.int$Location.ID > 30) & (pinedale.int$Location.ID < 99), ]
#write.csv(pinedale.int3, "data/pinedale.int3.raw.csv")
pinedale.int3 <- read.csv("data/pinedale.int3.raw.csv")
#pinedale.int2 <- pinedale.int[pinedale.int$Location.ID > 98, ]
#write.csv(pinedale.int2, "data/pinedale.int2.raw.csv")
pinedale.int2 <- read.csv("data/pinedale.int2.raw.csv")


## 3.5H fix (and add NA raw for missing data)  DONE - #### skip this and read processed int35 from file -----------------------------------------------
# aka. add minute info and add placeholder
unique (pinedale.int35$interval)  # so only 3, 4, 7; meaning at most missing one point. Yet ...3 7.. is different from ...4 7..
pinedale.int35$interval <- as.numeric(as.character(unlist((pinedale.int35$interval))))
pinedale.int35$index <- seq.int(nrow(pinedale.int35))  # add index so that adding rows will not mess up the loop
## Function for adding rows with ID and timestamps as placeholder
insert.na.row.35 <- function (index, dataframe) {
  missing.time <- vector()
  print(index)
  if (dataframe$interval[index-1] == 3) {
    dataframe$time[index] <- dataframe$time[index] + 0.5*60*60 
    missing.time <- dataframe$time[index]-3.5*60*60
  }
  if (dataframe$interval[index-1] == 4) {
    missing.time <- dataframe$time[index]-3.5*60*60
  }
  add.rows <- data.frame(dataframe$Location.ID[index], 0,0,0,0,0,0,0,0,0,0,missing.time, 0, 0)
  colnames(add.rows) <- colnames(dataframe)
  dataframe1 <- full_join(dataframe[1:index-1,], add.rows)
  dataframe <- full_join(dataframe1, dataframe[index:nrow(dataframe),])
  return(dataframe)
}

missing.count <- 0

for (i in 1:nrow(pinedale.int35)) {
  n <- which(pinedale.int35$index == i)
  if (pinedale.int35$interval[n] == 3) {
    pinedale.int35$time[n]  <- pinedale.int35$time[n] + 30*60  #30min
  }
  if  (pinedale.int35$interval[n] == 7) {
    pinedale.int35 <- insert.na.row.35 (n, pinedale.int35)
    missing.count <- missing.count + 1
  }
}

# since the lengh of pinedale.int35 changed with the added rows, run this again to make sure the last several

# #check whether all become 3.5
# pinedale.int35.check <- pinedale.int35 %>%
#   group_by(Location.ID) %>%
#   mutate (interval.new = difftime(time, lag(time, default = 0)))
# pinedale.int35.check <- pinedale.int35.check[duplicated(pinedale.int35$Location.ID),]
# unique(pinedale.int35.check$interval.new)

pinedale.int35 <- as.data.frame(pinedale.int35[, 1:12])
for (i in 1:nrow(pinedale.int35)) {
  if (pinedale.int35$Year[i] == 0) {
    pinedale.int35[i,2:11] <- NA
  }
}
pinedale.int35$time <- as.POSIXct(strptime(as.character(pinedale.int35$time), "%Y-%m-%d %H:%M:%S"))
# write.csv(pinedale.int35, "data/pinedale.int35.csv")


## 3H and add NA raw for missing data -----------------------------------------------
## check the missing data situation 
#unique(pinedale.int3$interval)   
## so it's either 6, missing one point, or 15, missing 4 points
pinedale.int3$time <- as.POSIXct(strptime(as.character(pinedale.int3$time), "%Y-%m-%d %H:%M"))
pinedale.int3 <- pinedale.int3[,2:14]
pinedale.int3$interval <- as.numeric(as.character(unlist((pinedale.int3$interval))))
pinedale.int3$index <- seq.int(nrow(pinedale.int3))

## THIS IS THE FINAL UNIVERSAL FUNCTION FOR THE DATASET. If you know the expected interval (needs to be an integer), you can detect the missing data and add NA rows to the data frame.
insert.na.row <- function (new.row, dataframe, interval) {
  missing.time <- vector()
  print(new.row)
  if (dataframe$Location.ID[new.row] == dataframe$Location.ID[new.row - 1]) {   #if they are from the same animal
    t <- dataframe$interval[new.row]/interval - 1  #see how many rows are missing
    missing.time.start <- dataframe$time[new.row]- t*interval*60*60+.0001
    missing.time.end <- dataframe$time[new.row] - interval*60*60+.0001
    missing.time <- as.character(seq(missing.time.start, missing.time.end, length.out = t))
    # if calculation happen to be 00:00:00, the time will be left out
    m0 <- matrix(0, nrow = t, ncol = 10)
    m1 <- matrix(0, nrow = t, ncol = 2)
    add.rows <- data.frame(cbind(rep(dataframe$Location.ID[new.row], t), m0, missing.time, m1))   #### need index column?
    colnames(add.rows) <- colnames(dataframe)
    cname <- colnames(add.rows[,1:11]) 
    add.rows[,cname] <- sapply(add.rows[,cname], as.character)
    add.rows[,cname] <- sapply(add.rows[,cname], as.numeric)  
    cname <- colnames(add.rows[,13:14]) 
    add.rows[,cname] <- sapply(add.rows[,cname], as.character)
    add.rows[,cname] <- sapply(add.rows[,cname], as.numeric)
    add.rows$time <- as.POSIXct(strptime(add.rows$time, "%Y-%m-%d %H:%M:%S"))
    dataframe1 <- bind_rows(dataframe[1:new.row-1,], add.rows)
    dataframe <- bind_rows(dataframe1, dataframe[new.row:nrow(dataframe),])
    return(dataframe)
  }
  else {
    print(new.row)
    print("It's the first row of one individual") #therefore just change it to 3 and move on
    dataframe$interval[new.row] <- interval
    return(dataframe)
  }
}

N <- nrow(pinedale.int3)
for (i in 2:N) {
  n <- which(pinedale.int3$index == i)  # n is the updated row number after NA row(s) being inserted, i is the original row number
  if  (pinedale.int3$interval[n] == 6 | pinedale.int3$interval[n] == 15) {
    pinedale.int3 <- insert.na.row (n, pinedale.int3, 3)
  }
}

#check whether all become 3
# pinedale.int3.check <- pinedale.int3 %>%
#   group_by(Location.ID) %>%
#   mutate (interval.new = difftime(time, lag(time, default = 0)))
# pinedale.int3.check <- pinedale.int3.check[duplicated(pinedale.int3.check$Location.ID),]
# unique(pinedale.int3.check$interval.new)  #somehow there's a NA but cannot really find it from the table

pinedale.int3 <- as.data.frame(pinedale.int3[,1:12])
for (i in 1:nrow(pinedale.int3)) {
  if (pinedale.int3$Year[i] == 0) {
    pinedale.int3[i,2:11] <- NA
  }
}
pinedale.int3$time <- as.POSIXct(strptime(as.character(pinedale.int3$time), "%Y-%m-%d %H:%M:%S"))
#write.csv(pinedale.int3, "data/pinedale.int3.csv")

## 2H and add NA raw for missing data -----------------------------------------------
unique(pinedale.int2$interval)   
## Manually edited the rows for 1 and 3, becoming "pinedale.int2.m.csv"
pinedale.int2 <- read.csv("data/pinedale.int2.m.csv")
pinedale.int2$time <- as.POSIXct(strptime(as.character(pinedale.int2$time), "%m/%d/%y %H:%M"))
pinedale.int2$X <- seq(from = 1, to = nrow(pinedale.int2)) #since I have manually deleted rows. calculate new seq #
pinedale.int2$interval <- as.numeric(as.character(unlist((pinedale.int2$interval))))
pinedale.int2 <- cbind(pinedale.int2[,2:14], pinedale.int2[,1])
colnames(pinedale.int2)[14] <- "index"

N <- nrow(pinedale.int2)
for (i in 2:N) {
  n <- which(pinedale.int2$index == i)
  if  (pinedale.int2$interval[n] !=2) {
    pinedale.int2 <- insert.na.row (n, pinedale.int2,2)
  }
}

# # check whether all become 2
# pinedale.int2.check <- pinedale.int2 %>%
#   group_by(Location.ID) %>%
#   mutate (interval.new = difftime(time, lag(time, default = 0)))
# pinedale.int2.check <- pinedale.int2.check[duplicated(pinedale.int2.check$Location.ID),]
# unique(pinedale.int2.check$interval.new)  #somehow there's a NA but cannot really find it from the table

pinedale.int2 <- as.data.frame(pinedale.int2[,1:12])
for (i in 1:nrow(pinedale.int2)) {
  if (pinedale.int2$Year[i] == 0) {
    pinedale.int2[i,2:11] <- NA
  }
}

pinedale.int2$time <- as.POSIXct(strptime(as.character(pinedale.int2$time), "%Y-%m-%d %H:%M:%S"))

# write.csv(pinedale.int2, "data/pinedale.int2.csv")

##### Unify FORMAT ########
pinedale.int35$Interval <- rep(3.5, nrow(pinedale.int35))
pinedale.int35 <- pinedale.int35[,-1]
pinedale.int3$Interval <- rep(3, nrow(pinedale.int3))
pinedale.int3 <- pinedale.int3[,-1]
pinedale.int2$Interval <- rep(2, nrow(pinedale.int2))
pinedale.int2 <-pinedale.int2[-1]

pinedale.all <- rbind(pinedale.int35, pinedale.int3)
pinedale.all <- rbind(pinedale.all, pinedale.int2)
#write.csv(pinedale.all, "data/PAPOlocationTableWX.csv") 


# Summarize PAPO data info by animal ID --------------------- 
pinedale.all$Location.ID <- as.factor(pinedale.all$Location.ID)

PAPOanimalTable <- data.frame()
for (i in levels(pinedale.all$Location.ID)) {
  sub.frame <- pinedale.all[pinedale.all$Location.ID == i,]
  Location.ID <- i
  Start.time <- sub.frame$time[1]
  End.time <- sub.frame$time[nrow(sub.frame)]
  Time.range <- difftime(End.time, Start.time) 
  Time.interval <- difftime(sub.frame$time[2], Start.time)
  Loc.num <- nrow(sub.frame)
  NA.num <- sum(is.na(sub.frame$Latitude))
  row.i <- cbind.data.frame(Location.ID, Start.time, End.time, Time.range, Time.interval, Loc.num, NA.num)
  PAPOanimalTable <- rbind.data.frame(PAPOanimalTable, row.i)
}

PAPOanimalTable$Location.ID <- as.numeric(as.character(PAPOanimalTable$Location.ID))
PAPOanimalTable <- arrange(PAPOanimalTable, Location.ID)

original.table <- read.csv("data/PAPOanimalTable.csv")
colnames(original.table)[1] <- "Location.ID"
PAPOanimalTableWX <- left_join(original.table, PAPOanimalTable, by = "Location.ID")
#write.csv(PAPOanimalTableWX, "data/PAPOanimalTableWX.csv")


# JMH dataset  ------------------------------------------------------------
# rockspring.raw <- read.csv("data/JMHlocationTable.csv")
# rockspring.raw$time <- paste0(rockspring.raw$Year,"-", rockspring.raw$Month,"-",rockspring.raw$"Day", " ", rockspring.raw$Hour, ":00")
# rockspring.raw$time <- as.POSIXct(strptime(as.character(rockspring.raw$time), "%Y-%m-%d %H:%M"))
# 
# sum(duplicated(paste(rockspring.raw$time, rockspring.raw$Location.ID)))
# # seems like most of the data is duplicated. To test which one is NOT duplicated:
# for (i in unique(rockspring.raw$Location.ID)) {
#   rockspring.sub <- rockspring.raw[rockspring.raw$Location.ID == i,]
#   n <- nrow(rockspring.sub)
#   m <- sum (duplicated(paste(rockspring.sub$time, rockspring.sub$Latitute)))
#   if (n != 2*m) {
#     print(paste(i, "total rows ", n, ", duplicate rows ", m))
#   } 
# }
# # ok now I see. 22 is not duplicated and all the other ones are doubled. And 22 shows up in the second half of the dataset So just pick the set 1-33. 
# dup <- sum(duplicated(paste(rockspring.raw$time, rockspring.raw$Location.ID)))
# tol <- nrow(rockspring.raw)
# rockspring.all <- rockspring.raw[-(1:dup),]
#write.csv(rockspring.all, "data/rockspring.all.csv")
# intervals?
##################### for check back, starting here ##########   ---------------------------------------
rockspring.all <- read.csv("data/rockspring.all.csv")
rockspring.all$time <- as.POSIXct(strptime(as.character(rockspring.all$time), "%Y-%m-%d %H:%M"))
rockspring.int <- select(rockspring.all, Location.ID, Year, Month, Day, Hour, Latitude, Longitude, Easting, Northing, Altitute, Horizontal, time)
rockspring.int <-rockspring.int %>%
  group_by(Location.ID) %>% 
  mutate (interval = difftime(time, lag(time, default = 0)))
rockspring.int <- rockspring.int[duplicated(rockspring.int$Location.ID),] 
# most of the 2 and 4 happen at the beginning of the collection of an ID
even <- c(which(rockspring.int$interval == 2), which(rockspring.int$interval == 4))
rockspring.int$index <- seq(1:nrow(rockspring.int))
for (i in even) {
  ID <- rockspring.int[rockspring.int$index==i,]$Location.ID
  rockspring.int[rockspring.int$index==i,]$interval <- 99 #just assign a random number
  row.number <- which(rockspring.int$interval == 99)
  subset <- subset.data.frame(rockspring.int, Location.ID == ID, select = interval)
  row.number.sub <- which(subset == 99)
  if (row.number.sub < 5) {
    rockspring.int <- rbind(rockspring.int[1:(row.number-row.number.sub),], rockspring.int[(row.number+1):nrow(rockspring.int), ])
  }
}
# now deal with the 3, 6, 9, 15, which only contain missing rows.
rockspring.int$index <- seq(1:nrow(rockspring.int))

N <- nrow(rockspring.int)
for (i in 2:N) {
  n <- which(rockspring.int$index == i)
  if  (rockspring.int$interval[n] != 3) {
    rockspring.int <- insert.na.row (n, rockspring.int, 3)
  }
}

# # check whether all become 3
# rockspring.int.check <- rockspring.int %>%
#   group_by(Location.ID) %>%
#   mutate (interval.new = difftime(time, lag(time, default = 0)))
# rockspring.int.check <- rockspring.int.check[duplicated(rockspring.int.check$Location.ID),]
# unique(rockspring.int.check$interval.new)

rockspring.int <- as.data.frame(rockspring.int[,1:12])
for (i in 1:nrow(rockspring.int)) {
  if (rockspring.int$Year[i] == 0) {
    rockspring.int[i,2:11] <- NA
  }
}
rockspring.int$time <- as.POSIXct(strptime(as.character(rockspring.int$time), "%Y-%m-%d %H:%M:%S"))
rockspring.int$interval <- rep(3, nrow(rockspring.int))
#write.csv(rockspring.int, "data/JMHlocationTableWX.csv")

# Summarize JMH data info by animal ID --------------------- 
rockspring.int$Location.ID <- as.factor(rockspring.int$Location.ID)

JMHanimalTable <- data.frame()
for (i in levels(rockspring.int$Location.ID)) {
  sub.frame <- rockspring.int[rockspring.int$Location.ID == i,]
  Location.ID <- i
  Start.time <- sub.frame$time[1]
  End.time <- sub.frame$time[nrow(sub.frame)]
  Time.range <- difftime(End.time, Start.time) 
  Time.interval <- difftime(sub.frame$time[2], Start.time)
  Loc.num <- nrow(sub.frame)
  NA.num <- sum(is.na(sub.frame$Latitude))
  row.i <- cbind.data.frame(Location.ID, Start.time, End.time, Time.range, Time.interval, Loc.num, NA.num)
  JMHanimalTable <- rbind.data.frame(JMHanimalTable, row.i)
}

JMHanimalTable$Location.ID <- as.numeric(as.character(JMHanimalTable$Location.ID))
JMHanimalTable <- arrange(JMHanimalTable, Location.ID)

original.table <- read.csv("data/JMHanimalTable.csv")
colnames(original.table)[1] <- "Location.ID"
JMHanimalTableWX <- left_join(original.table, JMHanimalTable, by = "Location.ID")
#write.csv(JMHanimalTableWX, "data/JMHanimalTableWX.csv")
