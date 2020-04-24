# this script is to examine whether # and/or types of encountering events has anything to do with migration dist
# did it on the flight from SFo to Cody. Results do not seem to be interesting.
library(ggplot2)
library(dplyr)
library(tidyr)

migdist <- read.csv(paste0(getwd(),"/FenceBehavior/DataTable/I2_MigDistbyNSD.csv"))
encounters <- read.csv(paste0(getwd(), "/FenceBehavior/DataTable/RESULTS/I2all_FB250_B4_P48_W7.csv"))

encounters.1 <- encounters %>% group_by(AnimalID) %>% summarise (N.events = n(),
                                                                 N.BNF = sum(eventTYPE =="Back-n-forth"), 
                                                                 N.Trace = sum(eventTYPE == "Trace"), 
                                                                 N.Trap = sum(eventTYPE == "Trap"),
                                                                 N.Bounce = sum(eventTYPE == "Bounce"))
  
encounters.2 <- left_join(encounters.1, migdist, by = "AnimalID") 
encounters.2 <- encounters.2 %>% filter(!is.na(MigDist))
encounters.2$AnimalID <- as.numeric(encounters.2$AnimalID)


plot.eventNdist <- ggplot(encounters.2, aes(MigDist, y = N.events)) +
  geom_point(aes(color = "red"), size = 1) + 
  labs ( x = "Mig Dist", y = "# of encounter events") +
  theme_bw()
plot.eventNdist

plot.BNFdist <- ggplot(encounters.2, aes(MigDist, y = N.BNF)) +
  geom_point(aes(color = "red"), size = 1) + 
  labs ( x = "Mig Dist", y = "# of Back-n-forth") +
  theme_bw() +
  theme(legend.position="none")
plot.BNFdist

plot.Trace <- ggplot(encounters.2, aes(MigDist, y = N.Trace)) +
  geom_point(aes(color = "red"), size = 1) + 
  labs ( x = "Mig Dist", y = "# of Trace") +
  theme_bw() +
  theme(legend.position="none")
plot.Trace

plot.Bounce <- ggplot(encounters.2, aes(MigDist, y = N.Bounce)) +
  geom_point(aes(color = "red"), size = 1) + 
  labs ( x = "Mig Dist", y = "# of Bounce") +
  theme_bw() +
  theme(legend.position="none")
plot.Bounce

encounters.3 <- encounters.2 %>% group_by(AnimalID) %>% summarise(bt.ratio = N.BNF/N.Trace)
encounters.3 <- left_join(encounters.3, migdist, by = "AnimalID")

plot.ratio <- ggplot(encounters.3, aes(MigDist, y = bt.ratio)) +
  geom_point(aes(color = "red"), size = 1) + 
  labs ( x = "Mig Dist", y = "back-n-forth/trace") +
  theme_bw() +
  theme(legend.position="none")
plot.ratio

ggplot(encounters.3, aes(bt.ratio, y = MigDist)) +
  geom_point(aes(color = "red"), size = 1) + 
  labs ( x = "back-n-forth/trace", y = "Mig Dist" ) +
  theme_bw() +
  theme(legend.position="none")

