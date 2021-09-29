#Clear R and the session
.rs.restartR()
rm(list = ls())

#Set seed for reproducibility
set.seed(2498)

#Load packages
library(sp);library(scales);library(raster);library(viridis);library(proj4);library(tidyverse);library(lubridate)
#Function form
GPSCleaner <- function() {
  datapath <- readline(prompt="Please enter the path to the GPS .txt files: ")
  deppath <- readline(prompt="Please enter the path to the deployment .csv file: ")
  toremove <- as.numeric(readline(prompt = "How many character to remove from file names to get the full ID? "))
  flist <- list.files(datapath, pattern = "*.txt")
  deployment <- read.csv(deppath, sep = ";")
  for(n in 1:length(flist)){
    depID <- substr(flist[n],1,nchar(flist[n])-toremove) #Extract the deployment ID from the file name
    print(depID)
    mydeploy <- deployment %>% filter(data_folder_name == depID)
    if(mydeploy$clutch_number == 1 &
       str_detect(mydeploy$Data_note, "stop", negate = T) &
       str_detect(mydeploy$Data_note, "sea", negate = T)){#Only first clutch, started on land and that did not stopped
      GPS <- read.table(paste0(datapath,"/",as.character(flist[n])))
      
      #Name the columns
      names(GPS) <- c("Dt","Latitude","Longitude","Altitude","Speed","Sat","Hdop","Sig")
      
      #Add new infos
      GPS$Dt <- with_tz(dmy_hms(GPS$Dt, tz="UTC"), tz="Australia/Melbourne")
      GPS$Datef <- date(GPS$Dt[1]) #or as.Date for UTC date
      GPS$Stage <- substr(factor(flist[n]),1,1)
      GPS$Nest <- substr(factor(flist[n]),2,5)
      GPS$Sex <- substr(factor(flist[n]),6,6)
      GPS <- GPS[with(GPS,order(Dt)),]
      rownames(GPS) <- 1:nrow(GPS)
      
      #Define colony from map
      colon <-  145.1503  #colony longititude 145.1496 ~ 145.151
      colat <-  -38.5103  #colony latitude -38.5106 ~ -38.509
      GPS$Cdist <- pointDistance(c(colon,colat),cbind(GPS$Longitude,GPS$Latitude),lonlat=T)
      
      #Plots to check the data
      par(mfrow=c(2,2))
      plot(GPS$Dt,col=topo.colors(nrow(GPS)))
      plot(GPS$Longitude,GPS$Latitude,type="l")
      points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
      plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
      plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
      
      OriGPS <- GPS
      
      checktrack <- readline(prompt = "Clean this track? (y/n) ")
      if(checktrack == "y"){
        #Remove manually abnormal positions
        abnormal <- readline(prompt = "Abnormal positions? (y/n) ")
        while(abnormal == "y"){
          minlat <- readline(prompt = "Minimal latitude to keep? ")
          maxlat <- readline(prompt = "Maximal latitude to keep? ")
          minlon <- readline(prompt = "Minimal longitude to keep? ")
          maxlon <- readline(prompt = "Maximal longitude to keep? ")
          GPS <- GPS %>% filter(Latitude >= minlat & Latitude <= max & Longitude >= minlon & Longitude <= maxlon)
          
          #Plots to check the data
          par(mfrow=c(2,2))
          plot(GPS$Dt,col=topo.colors(nrow(GPS)))
          plot(GPS$Longitude,GPS$Latitude,type="l")
          points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
          plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
          plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
          
          abnormal <- readline(prompt = "Abnormal positions? (y/n)")
        }
        
        trips <- readline(prompt = "Multiple trips? (y/n) ")
        if(trips == "y"){
          ntrips <- as.numeric(readline(prompt = "How many? "))
        }
        else{ntrips <- 1}
        
        for (q in 1:ntrips) {
          if(ntrips > 1){
            print(paste0("Trip n°", q))
            GPS <- OriGPS
            #Plots to check the data
            par(mfrow=c(2,2))
            plot(GPS$Dt,col=topo.colors(nrow(GPS)))
            plot(GPS$Longitude,GPS$Latitude,type="l")
            points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
            plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
            plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
          }
          
          satisfaction <- "n"
          GPS0 <- GPS
          
          while(satisfaction != "y"){
            #To check start and beginning of trip
            dispstart <- as.numeric(readline(prompt = "Number of data points to display at the beginning: "))
            dispend <- as.numeric(readline(prompt = "Number of data points to display at the end: "))
            dispend <- as.numeric(dispend)
            par(mfrow=c(1,2))
            plot(GPS$Cdist[1:dispstart]) #Check the start of the trip
            plot(GPS$Cdist[(nrow(GPS)-dispend):nrow(GPS)]) #Check the end of the trip
            
            delstart <- as.numeric(readline(prompt = "Delete how many data points at the start? "))
            delend <- as.numeric(readline(prompt = "Delete how many data points at the end? "))
            #print(nrow(GPS))
            
            # Delete before the trip
            GPS <- GPS[delstart:nrow(GPS),]
            rownames(GPS) <- 1:nrow(GPS)
            #print(nrow(GPS))
            
            # Delete after the trip
            GPS <- GPS[1:(nrow(GPS)-delend),]
            #print(nrow(GPS))
            
            #To check the deletion
            par(mfrow=c(1,1))
            plot(GPS$Cdist)
            
            satisfaction <- readline(prompt = "Satisfied? (y/n) ")
            if(satisfaction != "y"){
              restore <- readline(prompt = "Restore the GPS track? (y/n) ")
              if(restore == "y"){
                GPS <- GPS0
              }
            }
          }
          
          par(mfrow=c(2,2))
          #Distance between points (m)
          GPS$Dist <- rep(as.numeric(0), nrow(GPS))
          for (i in 1:(nrow(GPS)-1)) {
            GPS$Dist[i] <- pointDistance(c(GPS$Longitude[i], GPS$Latitude[i]), c(GPS$Longitude[i+1], GPS$Latitude[i+1]),lonlat=T)  
          }
          plot(GPS$Dt,GPS$Dist)
          
          #Time between two points (hours)
          GPS$Int <- rep(as.numeric(0), nrow(GPS))
          for (i in 1:(nrow(GPS)-1)) {
            GPS$Int[i] <- as.numeric(difftime(GPS$Dt[i+1], GPS$Dt[i], tz='GMT', units='hours'))
          }
          plot(GPS$Dt,GPS$Int)
          
          #Speed between two points (meters/hours) 
          GPS$Mspeed <- rep(as.numeric(0), nrow(GPS))
          GPS$Mspeed <- GPS$Dist/GPS$Int
          GPS$Mspeed[nrow(GPS)] <- 0
          plot(GPS$Dt,GPS$Mspeed,col=topo.colors(nrow(GPS)))
          plot(GPS$Longitude, GPS$Latitude, type="o", col=topo.colors(nrow(GPS)))
          
          #CLEANING: find and remove some points >7 000 m/h & Int <0.002 h (7.2s)
          GPS$Cdist[(GPS$Int<0.002)&(GPS$Mspeed>7000)] <- NA
          GPS <- na.omit(GPS)
          
          #Re-cal Mspeed
          rownames(GPS)<-1:nrow(GPS)
          for (i in 1:(nrow(GPS)-1)) {
            GPS$Dist[i] <- pointDistance(c(GPS$Longitude[i], GPS$Latitude[i]), c(GPS$Longitude[i+1], GPS$Latitude[i+1]),lonlat=T)
            GPS$Int[i] <- as.numeric(difftime(GPS$Dt[i+1], GPS$Dt[i], tz='GMT', units='hours'))
          }
          GPS$Mspeed <- GPS$Dist/GPS$Int
          GPS$Mspeed[nrow(GPS)] <- 0
          
          #Find and remove second points >8 000 m/h
          par(mfrow=c(1,1))
          plot(GPS$Dt,GPS$Mspeed)
          for (i in 2:(nrow(GPS))) {
            if(GPS$Mspeed[i-1]>8000 & GPS$Mspeed[i]>8000) {
              GPS$Cdist[i] <- NA
              GPS$Mspeed[i] <- 0}
          }
          while(any(is.na(GPS))){ # if TRUE=0 then go to #NEXT#
            GPS<-na.omit(GPS)
            #re-cal Mspeed
            rownames(GPS)<-1:nrow(GPS)
            for (i in 1:(nrow(GPS)-1)) {
              GPS$Dist[i] <- pointDistance(c(GPS$Longitude[i], GPS$Latitude[i]), c(GPS$Longitude[i+1], GPS$Latitude[i+1]),lonlat=T)
              GPS$Int[i] <- as.numeric(difftime(GPS$Dt[i+1], GPS$Dt[i], tz='GMT', units='hours'))
            }
            GPS$Mspeed <- GPS$Dist/GPS$Int
            GPS$Mspeed[nrow(GPS)] <- 0
            for (i in 2:(nrow(GPS))) {
              if(GPS$Mspeed[i-1]>8000 & GPS$Mspeed[i]>8000) {
                GPS$Cdist[i] <- NA
                GPS$Mspeed[i] <- 0}
            }
          }
          
          print(table(is.na(GPS)))
          plot(GPS$Dt,GPS$Mspeed)
          
          #NEXT# find and remove single points >8 000 m/h
          for (i in 2:(nrow(GPS)-1)) {
            if(GPS$Mspeed[i]>8000) { 
              if(GPS$Mspeed[i-1]>GPS$Mspeed[i+1]) {
                GPS$Cdist[i] <- NA}
              else{
                GPS$Cdist[i+1] <- NA}
            }
          }
          while(any(is.na(GPS))){#Repeat until all deleted
            GPS<-na.omit(GPS)
            #Re-cal Mspeed
            rownames(GPS)<-1:nrow(GPS)
            for (i in 1:(nrow(GPS)-1)) {
              GPS$Dist[i] <- pointDistance(c(GPS$Longitude[i], GPS$Latitude[i]), c(GPS$Longitude[i+1], GPS$Latitude[i+1]),lonlat=T)
              GPS$Int[i] <- as.numeric(difftime(GPS$Dt[i+1], GPS$Dt[i], tz='GMT', units='hours'))
            }
            GPS$Mspeed <- GPS$Dist/GPS$Int
            GPS$Mspeed[nrow(GPS)] <- 0
            #Remove points
            for (i in 2:(nrow(GPS)-1)) {
              if(GPS$Mspeed[i]>8000) { 
                if(GPS$Mspeed[i-1]>GPS$Mspeed[i+1]) {
                  GPS$Cdist[i] <- NA}
                else{
                  GPS$Cdist[i+1] <- NA}
              }
            }
          }
          
          plot(GPS$Dt,GPS$Mspeed)
          print(table(is.na(GPS)))
          
          #remove manually
          # GPS<-GPS[-53,]
          # GPS<-GPS[-(168:172),]
          # GPS<-GPS[c(-4629,-4626,-4623,-4621,-4619),]
          
          #SAVE GPS data as RData (& csv)
          if(ntrips == 1){
            saveRDS(GPS, file=paste0(datapath,"/Cleaned/GPS_",depID,".rds"))
            #write.csv(GPS, file=paste("GPS_",GPS$Stage[1],GPS$Nest[1],GPS$Sex[1],".csv",sep=""), row.names = F, quote = F) 
          }
          else{
            saveRDS(GPS, file=paste0(datapath,"/Cleaned/GPS_",depID,"_",q,".rds"))
          }
        }
      }
    }
    else{print("GPS track not complete")}
    # When start or/and end were missing, add points from weigh bridge data, time of departure/arrival, position of weigh bridge # 
    # {GPS$V2 <- as.character(GPS$V2)
    #   GPS[nrow(GPS)+1,] <- c("19/11/2018","16:05:00",-38.5103,145.1503,0,0,0,0,0)
    #   GPS$V2 <- as.factor(GPS$V2)
    #   GPS$V3 <- as.numeric(GPS$V3)
    #   GPS$V4 <- as.numeric(GPS$V4)
    #   tail(GPS)}
  }
}


#old####
#Load data
#Need to change datapath depending of the year to have the folder containing all GPS tracks of the season
datapath <- "Data/LPPI2020/GPStxt"
flist <- list.files(datapath, pattern = "*.txt")
n <- 1
flist[n]
GPS <- read.table(paste0(datapath,"/",as.character(flist[n])))

# When start or/and end were missing, add points from weigh bridge data, time of departure/arrival, position of weigh bridge # 
# {GPS$V2 <- as.character(GPS$V2)
#   GPS[nrow(GPS)+1,] <- c("19/11/2018","16:05:00",-38.5103,145.1503,0,0,0,0,0)
#   GPS$V2 <- as.factor(GPS$V2)
#   GPS$V3 <- as.numeric(GPS$V3)
#   GPS$V4 <- as.numeric(GPS$V4)
#   tail(GPS)}

#Name the columns
names(GPS) <- c("Dt","Latitude","Longitude","Altitude","Speed","Sat","Hdop","Sig")

GPS$Dt <- with_tz(dmy_hms(GPS$Dt, tz="UTC"), tz="Australia/Melbourne")
GPS$Datef <- date(GPS$Dt[1]) #or as.Date for UTC date
GPS$Stage <- substr(factor(flist[n]),1,1)
GPS$Nest <- substr(factor(flist[n]),2,5)
GPS$Sex <- substr(factor(flist[n]),6,6)
GPS <- GPS[with(GPS,order(Dt)),]
rownames(GPS) <- 1:nrow(GPS)

#Define colony from map
colon <-  145.1503  #colony longititude 145.1496 ~ 145.151
colat <-  -38.5103  #colony latitude -38.5106 ~ -38.509
GPS$Cdist <- pointDistance(c(colon,colat),cbind(GPS$Longitude,GPS$Latitude),lonlat=T)

par(mfrow=c(2,2))
plot(GPS$Dt,col=topo.colors(nrow(GPS)))
plot(GPS$Longitude,GPS$Latitude,type="l")
points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
plot(GPS$Cdist,col=topo.colors(nrow(GPS)))

GPS0 <- GPS #Save the original track if error

#Remove manually abnormal positions
# GPS[GPS$Latitude>-38.2,]
# GPS <- GPS[-709,]
# rownames(GPS) <- 1:nrow(GPS)
#

#To check
par(mfrow=c(1,2))
plot(GPS$Cdist[1:50]) #Check the start of the trip
plot(GPS$Cdist[(nrow(GPS)-500):nrow(GPS)]) #Check the end of the trip

# Delete before the trip
GPS <- GPS[20:nrow(GPS),]
rownames(GPS) <- 1:nrow(GPS)

# Delete after the trip
GPS <- GPS[1:(nrow(GPS)-400),]

#To check the deletion
par(mfrow=c(1,2))
plot(GPS$Cdist[1:50]) #Check the start of the trip
plot(GPS$Cdist[(nrow(GPS)-500):nrow(GPS)]) #Check the end of the trip
head(GPS,n=10)
tail(GPS,n=10)

par(mfrow=c(2,2))
#Distance between points (m)
GPS$Dist <- rep(as.numeric(0), nrow(GPS))
for (i in 1:(nrow(GPS)-1)) {
  GPS$Dist[i] <- pointDistance(c(GPS$Longitude[i], GPS$Latitude[i]), c(GPS$Longitude[i+1], GPS$Latitude[i+1]),lonlat=T)  
}
plot(GPS$Dt,GPS$Dist)

#Time between two points (hours)
GPS$Int <- rep(as.numeric(0), nrow(GPS))
for (i in 1:(nrow(GPS)-1)) {
  GPS$Int[i] <- as.numeric(difftime(GPS$Dt[i+1], GPS$Dt[i], tz='GMT', units='hours'))
}
plot(GPS$Dt,GPS$Int)

#Speed between two points (meters/hours) 
GPS$Mspeed <- rep(as.numeric(0), nrow(GPS))
GPS$Mspeed <- GPS$Dist/GPS$Int
GPS$Mspeed[nrow(GPS)] <- 0
plot(GPS$Dt,GPS$Mspeed,col=topo.colors(nrow(GPS)))
plot(GPS$Longitude, GPS$Latitude, type="o", col=topo.colors(nrow(GPS)))

#CLEANING: find and remove some points >7 000 m/h & Int <0.002 h (7.2s)
table(GPS$Mspeed>7000)
table(GPS$Int<0.002)  #Last point is always TRUE 
table((GPS$Int<0.002)&(GPS$Mspeed>7000))
GPS0 <- GPS
GPS$Cdist[(GPS$Int<0.002)&(GPS$Mspeed>7000)] <- NA
table(is.na(GPS))
GPS <- na.omit(GPS)

#Re-cal Mspeed
rownames(GPS)<-1:nrow(GPS)
for (i in 1:(nrow(GPS)-1)) {
  GPS$Dist[i] <- pointDistance(c(GPS$Longitude[i], GPS$Latitude[i]), c(GPS$Longitude[i+1], GPS$Latitude[i+1]),lonlat=T)
  GPS$Int[i] <- as.numeric(difftime(GPS$Dt[i+1], GPS$Dt[i], tz='GMT', units='hours'))
}

GPS$Mspeed <- GPS$Dist/GPS$Int
GPS$Mspeed[nrow(GPS)] <- 0

#Find and remove second points >8 000 m/h
par(mfrow=c(1,1))
plot(GPS$Dt,GPS$Mspeed)
table(GPS$Mspeed>8000)
for (i in 2:(nrow(GPS))) {
  if(GPS$Mspeed[i-1]>8000 & GPS$Mspeed[i]>8000) {
    GPS$Cdist[i] <- NA
    GPS$Mspeed[i] <- 0}
}
table(is.na(GPS)) 
while(any(is.na(GPS))){ # if TRUE=0 then go to #NEXT#
  GPS<-na.omit(GPS)
  #re-cal Mspeed
  rownames(GPS)<-1:nrow(GPS)
  for (i in 1:(nrow(GPS)-1)) {
    GPS$Dist[i] <- pointDistance(c(GPS$Longitude[i], GPS$Latitude[i]), c(GPS$Longitude[i+1], GPS$Latitude[i+1]),lonlat=T)
    GPS$Int[i] <- as.numeric(difftime(GPS$Dt[i+1], GPS$Dt[i], tz='GMT', units='hours'))
  }
  GPS$Mspeed <- GPS$Dist/GPS$Int
  GPS$Mspeed[nrow(GPS)] <- 0
  for (i in 2:(nrow(GPS))) {
    if(GPS$Mspeed[i-1]>8000 & GPS$Mspeed[i]>8000) {
      GPS$Cdist[i] <- NA
      GPS$Mspeed[i] <- 0}
  }
}

#NEXT# find and remove single points >8 000 m/h
plot(GPS$Dt,GPS$Mspeed)
table(GPS$Mspeed>8000)
for (i in 2:(nrow(GPS)-1)) {
  if(GPS$Mspeed[i]>8000) { 
    if(GPS$Mspeed[i-1]>GPS$Mspeed[i+1]) {
      GPS$Cdist[i] <- NA}
    else{
      GPS$Cdist[i+1] <- NA}
  }
}
table(is.na(GPS))
while(any(is.na(GPS))){#Repeat until all deleted
  GPS<-na.omit(GPS)
  #Re-cal Mspeed
  rownames(GPS)<-1:nrow(GPS)
  for (i in 1:(nrow(GPS)-1)) {
    GPS$Dist[i] <- pointDistance(c(GPS$Longitude[i], GPS$Latitude[i]), c(GPS$Longitude[i+1], GPS$Latitude[i+1]),lonlat=T)
    GPS$Int[i] <- as.numeric(difftime(GPS$Dt[i+1], GPS$Dt[i], tz='GMT', units='hours'))
  }
  GPS$Mspeed <- GPS$Dist/GPS$Int
  GPS$Mspeed[nrow(GPS)] <- 0
  #Remove points
  for (i in 2:(nrow(GPS)-1)) {
    if(GPS$Mspeed[i]>8000) { 
      if(GPS$Mspeed[i-1]>GPS$Mspeed[i+1]) {
        GPS$Cdist[i] <- NA}
      else{
        GPS$Cdist[i+1] <- NA}
    }
  }
}
plot(GPS$Dt,GPS$Mspeed)

#remove manually
# GPS<-GPS[-53,]
# GPS<-GPS[-(168:172),]
# GPS<-GPS[c(-4629,-4626,-4623,-4621,-4619),]

#SAVE GPS data as RData (& csv)
saveRDS(GPS, file=paste0(datapath,"/Cleaned/GPS_",GPS$Stage[1],GPS$Nest[1],GPS$Sex[1],".rds"))
#write.csv(GPS, file=paste("GPS_",GPS$Stage[1],GPS$Nest[1],GPS$Sex[1],".csv",sep=""), row.names = F, quote = F)
#####








