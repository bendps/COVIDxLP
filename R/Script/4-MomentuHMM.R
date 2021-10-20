#~~~~~#
rm(list = ls())
set.seed(2498)
library(tidyverse);library(momentuHMM)
#~~~~~#

crwOut <- readRDS("Data/LPPI2019/Interpolated_GPS_Depth_2019.rds")

lpdata <- prepData(crwOut)

#Exploration of the data
hist(lpdata$step, breaks = 20)
hist(lpdata$angle)
hist(lpdata$prop_dive)
plot(lpdata,compact=T)

#Label states
stateNames <- c("Foraging","Transit","Mix")

#Distributions for observation processes
dist <- list(step = "gamma", angle = "vm", prop_dive = "gamma")

#Initial parameters for gamma and von Mises distributions
mu0 <- c(100,800,800) #Step mean 
sigma0 <- c(50,100,100) #Step SD
#zeromass0 <- c(0.5,1) #Step zero-mass
stepPar0 <- c(mu0,sigma0)

angleMean0 <- c(0.1,0.1,0.1) # angle mean
#kappa0 <- c(1,1,1) # angle concentration
anglePar0 <- c(angleMean0)

mu0 <- c(0.5,0.1,0.3) #Dive mean 
sigma0 <- c(0.1,0.01,0.1) #Dive SD
zeromass0 <- c(0.01,0.1,0.05) #Dive zero-mass
divePar0 <- c(mu0,sigma0,zeromass0)

Par0_m1 <- list(step = stepPar0, angle = anglePar0, prop_dive = divePar0)

#Call to fitting function
m1 <- fitHMM(data = lpdata, nbStates = 3, dist = dist, Par0 = Par0_m1, stateNames = stateNames)

plot(m1)
