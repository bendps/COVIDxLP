#~~~~~#
rm(list = ls())
set.seed(2498)
library(sp);library(scales);library(raster)
library(viridis);library(proj4);library(tidyverse)
library(lubridate);library(suncalc)
#~~~~~#

#Input parameters
<<<<<<< HEAD
myseason <- 2014 #What year do you want to clean?
toremove <- 14 # Characters to remove from file names to get the full ID

datapath <- paste0("Data/LPPI",myseason,"/GPS") #Path to the GPS files
deppath <- paste0("Data/LPPI",myseason,"/Deployment_LP_",myseason,".csv") # Path to the deployment .csv file
=======
myseason <- 2017 #What year do you want to clean?
datapath <- "Data/LPPI2017/GPS" #Path to the GPS files
deppath <- "Data/LPPI2017/Deployment_LP_2017.csv" # Path to the deployment .csv file
toremove <- 4 # Characters to remove from file names to get the full ID
>>>>>>> 955f76970b1a130701cddc5e6b685d6e59902961

flist <- list.files(datapath, pattern = "*.txt")
if(myseason <= 2017 | myseason == 2012 | myseason == 2013){
  flist <- list.files(datapath, pattern = "*.csv")
}
if(myseason == 2015){
  flist <- flist[-which(!grepl("\\D", substr(flist,1,5)))] #remove bird that are not from the parade
}

if(myseason >= 2017 | myseason == 2010 | myseason == 2011 | myseason == 2012 | myseason == 2013 | myseason == 2014){
  deployment <- read.csv(deppath, sep = ";")
}

weighbridge <- readRDS("Data/weighbridge.rds")

<<<<<<< HEAD
for(n in 31:length(flist)){
  print(paste0(n,"/", length(flist)))
  #Get deployment and weighbridge data
  if(myseason == 2015){
    depID <- substr(flist[n], 6, 11)
    print(depID)
    mydeploy <- tibble(clutch_number = 1)
    mybridge <- weighbridge %>% filter(pit_tag == depID & season == myseason)
  }else if(myseason == 2017){
=======
for(n in 41:length(flist)){
  print(paste0(n,"/", length(flist)))
  #Get deployment and weighbridge data
  if(myseason <= 2017){
>>>>>>> 955f76970b1a130701cddc5e6b685d6e59902961
    depID <- str_replace_all(str_extract_all(flist, "_\\s*(.*?)\\s*_")[[n]][2], "_", "")
    print(depID)
    mydeploy <- deployment %>% filter(data_folder_name == depID)
    mybridge <- weighbridge %>% filter(pit_tag == depID & season == myseason)
  }else{
    depID <- substr(flist[n],1,nchar(flist[n])-toremove) 
    print(depID)
    mydeploy <- deployment %>% filter(data_folder_name == depID)
    if(myseason == 2014){names(mydeploy)[5] <- "Transp"}
    if(myseason == 2012 | myseason == 2013 | myseason == 2014){
      mybridge <- weighbridge %>% filter(pit_tag == mydeploy$Transp & season == myseason)
    }else{mybridge <- weighbridge %>% filter(pit_tag == str_sub(mydeploy$ID[1], start= -6) & season == myseason)}
  }
  
  #Load GPS
  if(mydeploy$clutch_number != 2 & myseason > 2017){
    GPS <- read.table(paste0(datapath,"/",as.character(flist[n])))
  }else if(mydeploy$clutch_number != 2 & myseason == 2017){
    GPS <- read.csv(paste0(datapath,"/",as.character(flist[n])), sep = ",", skip = 6)
  }else if(myseason <= 2015){
    GPS <- read.csv(paste0(datapath,"/",as.character(flist[n])), sep = ",")
  }
  
  if(mydeploy$clutch_number != 2){
<<<<<<< HEAD
    #Name the columns & add infos
    if(myseason == 2010 | myseason == 2011 | myseason == 2012 | myseason == 2013 | myseason == 2014){
      names(GPS) <- c("Date", "Time", "Latitude","Longitude","Altitude", "Speed","Sat","Hdop","Sig", "TFFs")
      GPS$Dt <- paste(GPS$Date, GPS$Time)
      GPS <- GPS[,c(3:11)]
      GPS <- GPS %>% relocate(Dt, .before = Latitude)
      GPS <- GPS %>% dplyr::select(-TFFs)
      GPS$Dt <- ymd_hms(GPS$Dt, tz="Australia/Melbourne")
      GPS$Datef <- date(GPS$Dt) #or as.Date for UTC date
      GPS$Stage <- mydeploy$Stage
      GPS$Nest <- mydeploy$Nest
      GPS$Sex <- mydeploy$Sex
    }
    if(myseason == 2015){
      names(GPS) <- c("Date", "Time", "Latitude","Longitude","Altitude", "Speed","Sat","Hdop","Sig", "TFFs")
      GPS$Dt <- paste(GPS$Date, GPS$Time)
      GPS <- GPS[,c(3:11)]
      GPS <- GPS %>% relocate(Dt, .before = Latitude)
      GPS <- GPS %>% dplyr::select(-TFFs)
      GPS$Dt <- dmy_hms(GPS$Dt, tz="Australia/Melbourne")
      GPS$Datef <- date(GPS$Dt) #or as.Date for UTC date
      GPS$Stage <- str_replace_all(str_extract(flist[n], "_(.s*?)\\s*"),"_", "")
      GPS$Nest <- substr(flist[n], 1, 4)
      if(str_detect(flist[n], "F") == TRUE){
        GPS$Sex <- "F"
      }else{GPS$Sex <- "M"}
    }
    if(myseason == 2017){
      names(GPS) <- c("Date", "Time", "Latitude","Longitude","Altitude","Sat","Hdop","Sig", "TFFs", "Speed")
      GPS$Dt <- paste(GPS$Date, GPS$Time)
      GPS <- GPS[,c(3:11)]
      GPS <- GPS %>% relocate(Dt, .before = Latitude)
=======
    #Name the columns
    if(myseason <= 2017){
      names(GPS) <- c("Date", "Time", "Latitude","Longitude","Altitude","Sat","Hdop","Sig", "TFFs", "Speed")
      GPS$Dt <- paste(GPS$Date, GPS$Time)
      GPS <- GPS[,c(3:11)]
      GPS <- GPS %>% relocate(Dt, .before = Latitude)
>>>>>>> 955f76970b1a130701cddc5e6b685d6e59902961
      GPS <- GPS %>% relocate(Speed, .before = Sat)
      GPS <- GPS %>% dplyr::select(-TFFs)
      GPS$Dt <- with_tz(mdy_hms(GPS$Dt, tz="UTC"), tz="Australia/Melbourne")
      GPS$Datef <- date(GPS$Dt) #or as.Date for UTC date
      GPS$Stage <- str_replace_all(str_extract(flist[n], "(.s*?)\\s*_"),"_", "")
      GPS$Nest <- str_replace_all(str_extract_all(flist, "_\\s*(.*?)\\s*_")[[n]][1], "_", "")
      if(str_detect(flist[n], "F") == TRUE){
        GPS$Sex <- "F"
      }else{GPS$Sex <- "M"}
<<<<<<< HEAD
=======
      
      #Re-order the data to look like later years
      
      
>>>>>>> 955f76970b1a130701cddc5e6b685d6e59902961
    }
    #Add new infos
    if(myseason == 2018){
      names(GPS) <- c("Date", "Time", "Latitude","Longitude","Altitude","Speed","Sat","Hdop","Sig")
      GPS$Dt <- paste(GPS$Date, GPS$Time)
      GPS <- GPS[,c(3:10)]
      GPS <- GPS %>% relocate(Dt, .before = Latitude)
      GPS$Dt <- with_tz(dmy_hms(GPS$Dt, tz="UTC"), tz="Australia/Melbourne")
      GPS$Datef <- date(GPS$Dt) #or as.Date for UTC date
      GPS$Stage <- substr(factor(flist[n]),1,1)
      GPS$Nest <- substr(factor(flist[n]),2,5)
      GPS$Sex <- substr(factor(flist[n]),6,6)
    }
    if(myseason >= 2019){
      names(GPS) <- c("Dt","Latitude","Longitude","Altitude","Speed","Sat","Hdop","Sig")
      GPS$Dt <- with_tz(dmy_hms(GPS$Dt, tz="UTC"), tz="Australia/Melbourne")
      GPS$Datef <- date(GPS$Dt) #or as.Date for UTC date
      GPS$Stage <- substr(factor(flist[n]),1,1)
      GPS$Nest <- substr(factor(flist[n]),2,5)
      GPS$Sex <- substr(factor(flist[n]),6,6)
    }
    GPS <- GPS[with(GPS,order(Dt)),]
    rownames(GPS) <- 1:nrow(GPS)
    
    #Define colony from map
    colon <-  145.1503  #colony longitude 145.1496 ~ 145.151
    colat <-  -38.5103  #colony latitude -38.5106 ~ -38.509
    GPS$Cdist <- pointDistance(c(colon,colat),cbind(GPS$Longitude,GPS$Latitude),lonlat=T)
    
    #Get sunrise and sunset
    sun_times <- getSunlightTimes(date = as.Date(names(table(GPS$Datef))),
                                  lat = GPS$Latitude[1],
                                  lon = GPS$Longitude[1],
                                  keep = c("sunrise", "sunset"),
                                  tz = "Australia/Melbourne")
    
    #Plots to check the data
    par(mfrow=c(2,2))
    plot(GPS$Dt,col=topo.colors(nrow(GPS)))
    plot(GPS$Longitude,GPS$Latitude,type="l")
    points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
    points(colon, colat, pch = 10)
    plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
    abline(v=sun_times$sunrise, col = "blue")
    abline(v=sun_times$sunset, col = "red")
    plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
    
    OriGPS <- GPS
    
    checktrack <- readline(prompt = "Clean this track? (y/n/q) ")
    if(checktrack == "q"){stop("function stopped")}
    if(checktrack == "y"){
      #Remove manually abnormal positions
      abnormal <- readline(prompt = "Abnormal positions? (y/n) ")
      request <- "dummy"
      while(abnormal == "y" & request != "abort"){
        print("Penguin's deployment:")
        print(mydeploy)
        print("GPS data:")
        print(GPS[1,])
        print("Weighbridge data:")
        print(mybridge)
        request <- readline(prompt = "What is the issue? (beg/end/remove/keep/abort) ")
        if(request == "beg"){
          startime <- readline(prompt = "What should be the new starting date time? (yyyy-mm-dd hh:mm:ss) ")
          newstart <- data.frame(ymd_hms(startime, tz="Australia/Melbourne"),
                                 colat,
                                 colon,
                                 0,
                                 0,
                                 0,
                                 0,
                                 0,
                                 date(startime),
                                 GPS$Stage[1],
                                 GPS$Nest[1],
                                 GPS$Sex[1],
                                 0)
          names(newstart) <- names(GPS)
          GPS <- rbind(GPS, newstart)
          GPS <- GPS[with(GPS,order(Dt)),]
          rownames(GPS) <- 1:nrow(GPS)
        }
        if(request == "end"){
          endtime <- readline(prompt = "What should be the new ending date time? (yyyy-mm-dd hh:mm:ss) ")
          newend <- data.frame(ymd_hms(endtime, tz="Australia/Melbourne"),
                               colat,
                               colon,
                               0,
                               0,
                               0,
                               0,
                               0,
                               date(endtime),
                               GPS$Stage[1],
                               GPS$Nest[1],
                               GPS$Sex[1],
                               0)
          names(newend) <- names(GPS)
          GPS <- rbind(GPS, newend)
          GPS <- GPS[with(GPS,order(Dt)),]
          rownames(GPS) <- 1:nrow(GPS)
        }
        if(request == "keep"){
          minlat <- as.numeric(readline(prompt = "Minimal latitude to keep? "))
          maxlat <- as.numeric(readline(prompt = "Maximal latitude to keep? "))
          minlon <- as.numeric(readline(prompt = "Minimal longitude to keep? "))
          maxlon <- as.numeric(readline(prompt = "Maximal longitude to keep? "))
          GPS <- GPS %>% dplyr::filter(Latitude >= minlat & Latitude <= maxlat & Longitude >= minlon & Longitude <= maxlon)
          GPS <- GPS[with(GPS,order(Dt)),]
          rownames(GPS) <- 1:nrow(GPS)
        }
        if(request == "remove"){
          par(mfrow=c(1,1))
          minlim <- as.numeric(readline(prompt = "Minimal row n° to display? "))
          maxlim <- as.numeric(readline(prompt = "Maximal row n° to display? "))
          plot(GPS$Cdist[minlim:maxlim],col=topo.colors(nrow(GPS)))
          removed_row <- as.numeric(readline(prompt = "Index of the row to remove? "))
          GPS <- GPS[-removed_row,]
          GPS <- GPS[with(GPS,order(Dt)),]
          rownames(GPS) <- 1:nrow(GPS)
        }
        if(request == "abort"){
          next
          abnormal <- "n"}
        
        #Plots to check the data
        par(mfrow=c(2,2))
        plot(GPS$Dt,col=topo.colors(nrow(GPS)))
        plot(GPS$Longitude,GPS$Latitude,type="l")
        points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
        plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
        plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
        
        abnormal <- readline(prompt = "Abnormal positions? (y/n) ")
      }
      if(request == "abort"){
        next
      }
      
      night <- readline("Does the trip last more than 1 day? (y/n) ")
      if(night == ("y")){
        n_night <- as.numeric(readline("How many nights need to be removed? "))
        print(sun_times)
        for(g in 1:n_night){
          date_night <- dmy(readline(paste0("Date of night ", g, "? (d/m/y) ")))
          mysunset <- sun_times$sunset[which(date(sun_times$date) == date_night)]
          mysunrise <- sun_times$sunrise[which(date(sun_times$date) == date_night+1)]
          GPS <- GPS[-which(GPS$Dt > mysunset & GPS$Dt < mysunrise),]
          rownames(GPS) <- 1:nrow(GPS)
          
          #Plots to check the data
          par(mfrow=c(2,2))
          plot(GPS$Dt,col=topo.colors(nrow(GPS)))
          plot(GPS$Longitude,GPS$Latitude,type="l")
          points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
          points(colon, colat, pch = 10)
          plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
          abline(v=sun_times$sunrise, col = "blue")
          abline(v=sun_times$sunset, col = "red")
          plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
        }
      }
      
      OriGPS <- GPS
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
          points(colon, colat, pch=20)
          plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
          abline(v=sun_times$sunrise, col = "blue")
          abline(v=sun_times$sunset, col = "red")
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
          
          # Delete before the trip
          GPS <- GPS[delstart:nrow(GPS),]
          rownames(GPS) <- 1:nrow(GPS)
          
          # Delete after the trip
          GPS <- GPS[1:(nrow(GPS)-delend),]
          
          #To check the deletion
          par(mfrow=c(1,2))
          plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
          plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
          
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
        GPS <- GPS %>% drop_na(Int, Mspeed)
        
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
        while(any(is.na(GPS$Cdist))){ # if TRUE=0 then go to #NEXT#
          GPS <- GPS %>% drop_na(Cdist)
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
        
        print(table(is.na(GPS$Mspeed)))
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
        while(any(is.na(GPS$Cdist))){#Repeat until all deleted
          GPS <- GPS %>% drop_na(Cdist)
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
        print(table(is.na(GPS$Mspeed)))
        
        #SAVE GPS data as RData
<<<<<<< HEAD
        if(myseason <= 2017 & myseason > 2014){
=======
        if(myseason <= 2017){
>>>>>>> 955f76970b1a130701cddc5e6b685d6e59902961
          if(ntrips == 1){
            saveRDS(GPS, file=paste0(datapath,"/Cleaned/GPS_", GPS$Stage[1], GPS$Nest[1], GPS$Sex[1], depID,".rds"))
          }
          else{
            saveRDS(GPS, file=paste0(datapath,"/Cleaned/GPS_", GPS$Stage[1], GPS$Nest[1], GPS$Sex[1], depID, "_", q, ".rds"))
          }
        }else{
          if(ntrips == 1){
            saveRDS(GPS, file=paste0(datapath,"/Cleaned/GPS_",depID,".rds"))
          }
          else{
            saveRDS(GPS, file=paste0(datapath,"/Cleaned/GPS_",depID,"_",q,".rds"))
          }
<<<<<<< HEAD
        }

      }
    }
  }
  else{print("GPS track not complete")}
}

# For 2016, rds data ####
myseason <- 2016 #What year do you want to clean?

datapath <- paste0("Data/LPPI",myseason,"/GPS") #Path to the GPS files
weighbridge <- readRDS("Data/weighbridge.rds")
lp_gps <- readRDS("Data/LPPI2016/GPS/lp_GPS_2016.rds")

lp_gps <- lp_gps %>% filter(deployment_site == "Parade", clutch_no == 1) # filter the data to keep only pinp and first clutches
rownames(lp_gps) <- 1:nrow(lp_gps)
lp_gps$index <- as.numeric(rownames(lp_gps))
loop_index <- lp_gps$index[match(unique(lp_gps$input_file), lp_gps$input_file)] # get the first occurence of the tracks

mydeploy <- tibble(clutch_number = 1)

for (i in loop_index) {
  # reshape the gps file
  GPS <- lp_gps %>% filter(input_file == lp_gps$input_file[i])
  
  depID <- str_match(my_gps$input_file[1], "/\\s*(.*?)\\s*.csv")[,2] #get dep_id for Axy data
  mybridge <- weighbridge %>% filter(pit_tag == GPS$pit_tag[1] & season == myseason)
  
  GPS$Dt <- with_tz(GPS$timestamp_utc, tz = "Australia/Melbourne") # set australian tz
  GPS$Datef <- date(GPS$Dt)
  GPS$Stage <- substr(GPS$breeding_stage, 1, 1)
  
  GPS <- GPS %>% select(Dt, Latitude, Longitude, Altitude, Info, Satellites, HDOP, PDOP, Datef, Stage, burrow, sex)
  names(GPS) <- c("Dt", "Latitude", "Longitude", "Altitude", "Speed", "Sat", "Hdop", "Sig", "Datef", "Stage", "Nest", "Sex")
  GPS <- GPS[with(GPS,order(Dt)),]
  rownames(GPS) <- 1:nrow(GPS)
  
  #Define colony from map
  colon <-  145.1503  #colony longitude 145.1496 ~ 145.151
  colat <-  -38.5103  #colony latitude -38.5106 ~ -38.509
  GPS$Cdist <- pointDistance(c(colon,colat),cbind(GPS$Longitude,GPS$Latitude),lonlat=T)
  
  #Get sunrise and sunset
  sun_times <- getSunlightTimes(date = as.Date(names(table(GPS$Datef))),
                                lat = GPS$Latitude[1],
                                lon = GPS$Longitude[1],
                                keep = c("sunrise", "sunset"),
                                tz = "Australia/Melbourne")
  
  #Plots to check the data
  par(mfrow=c(2,2))
  plot(GPS$Dt,col=topo.colors(nrow(GPS)))
  plot(GPS$Longitude,GPS$Latitude,type="l")
  points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
  points(colon, colat, pch = 10)
  plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
  abline(v=sun_times$sunrise, col = "blue")
  abline(v=sun_times$sunset, col = "red")
  plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
  
  OriGPS <- GPS
  
  checktrack <- readline(prompt = "Clean this track? (y/n/q) ")
  if(checktrack == "q"){stop("function stopped")}
  if(checktrack == "y"){
    #Remove manually abnormal positions
    abnormal <- readline(prompt = "Abnormal positions? (y/n) ")
    request <- "dummy"
    while(abnormal == "y" & request != "abort"){
      print("Penguin's deployment:")
      print(mydeploy)
      print("GPS data:")
      print(GPS[1,])
      print("Weighbridge data:")
      print(mybridge)
      request <- readline(prompt = "What is the issue? (beg/end/remove/keep/abort) ")
      if(request == "beg"){
        startime <- readline(prompt = "What should be the new starting date time? (yyyy-mm-dd hh:mm:ss) ")
        newstart <- data.frame(ymd_hms(startime, tz="Australia/Melbourne"),
                               colat,
                               colon,
                               0,
                               0,
                               0,
                               0,
                               0,
                               date(startime),
                               GPS$Stage[1],
                               GPS$Nest[1],
                               GPS$Sex[1],
                               0)
        names(newstart) <- names(GPS)
        GPS <- rbind(GPS, newstart)
        GPS <- GPS[with(GPS,order(Dt)),]
        rownames(GPS) <- 1:nrow(GPS)
      }
      if(request == "end"){
        endtime <- readline(prompt = "What should be the new ending date time? (yyyy-mm-dd hh:mm:ss) ")
        newend <- data.frame(ymd_hms(endtime, tz="Australia/Melbourne"),
                             colat,
                             colon,
                             0,
                             0,
                             0,
                             0,
                             0,
                             date(endtime),
                             GPS$Stage[1],
                             GPS$Nest[1],
                             GPS$Sex[1],
                             0)
        names(newend) <- names(GPS)
        GPS <- rbind(GPS, newend)
        GPS <- GPS[with(GPS,order(Dt)),]
        rownames(GPS) <- 1:nrow(GPS)
      }
      if(request == "keep"){
        minlat <- as.numeric(readline(prompt = "Minimal latitude to keep? "))
        maxlat <- as.numeric(readline(prompt = "Maximal latitude to keep? "))
        minlon <- as.numeric(readline(prompt = "Minimal longitude to keep? "))
        maxlon <- as.numeric(readline(prompt = "Maximal longitude to keep? "))
        GPS <- GPS %>% dplyr::filter(Latitude >= minlat & Latitude <= maxlat & Longitude >= minlon & Longitude <= maxlon)
        GPS <- GPS[with(GPS,order(Dt)),]
        rownames(GPS) <- 1:nrow(GPS)
      }
      if(request == "remove"){
        par(mfrow=c(1,1))
        minlim <- as.numeric(readline(prompt = "Minimal row n° to display? "))
        maxlim <- as.numeric(readline(prompt = "Maximal row n° to display? "))
        plot(GPS$Cdist[minlim:maxlim],col=topo.colors(nrow(GPS)))
        removed_row <- as.numeric(readline(prompt = "Index of the row to remove? "))
        GPS <- GPS[-removed_row,]
        GPS <- GPS[with(GPS,order(Dt)),]
        rownames(GPS) <- 1:nrow(GPS)
      }
      if(request == "abort"){
        next
        abnormal <- "n"}
      
      #Plots to check the data
      par(mfrow=c(2,2))
      plot(GPS$Dt,col=topo.colors(nrow(GPS)))
      plot(GPS$Longitude,GPS$Latitude,type="l")
      points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
      plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
      plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
      
      abnormal <- readline(prompt = "Abnormal positions? (y/n) ")
    }
    if(request == "abort"){
      next
    }
    
    night <- readline("Does the trip last more than 1 day? (y/n) ")
    if(night == ("y")){
      n_night <- as.numeric(readline("How many nights need to be removed? "))
      print(sun_times)
      for(g in 1:n_night){
        date_night <- ymd(readline(paste0("Date of night ", g, "? (yyyy/mm/dd) ")))
        mysunset <- sun_times$sunset[which(date(sun_times$date) == date_night)]
        mysunrise <- sun_times$sunrise[which(date(sun_times$date) == date_night+1)]
        GPS <- GPS[-which(GPS$Dt > mysunset & GPS$Dt < mysunrise),]
        rownames(GPS) <- 1:nrow(GPS)
        
        #Plots to check the data
        par(mfrow=c(2,2))
        plot(GPS$Dt,col=topo.colors(nrow(GPS)))
        plot(GPS$Longitude,GPS$Latitude,type="l")
        points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
        points(colon, colat, pch = 10)
        plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
        abline(v=sun_times$sunrise, col = "blue")
        abline(v=sun_times$sunset, col = "red")
        plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
      }
    }
    
    OriGPS <- GPS
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
        points(colon, colat, pch=20)
        plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
        abline(v=sun_times$sunrise, col = "blue")
        abline(v=sun_times$sunset, col = "red")
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
        
        # Delete before the trip
        GPS <- GPS[delstart:nrow(GPS),]
        rownames(GPS) <- 1:nrow(GPS)
        
        # Delete after the trip
        GPS <- GPS[1:(nrow(GPS)-delend),]
        
        #To check the deletion
        par(mfrow=c(1,2))
        plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
        plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
        
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
      GPS <- GPS %>% drop_na(Int, Mspeed)
      
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
      while(any(is.na(GPS$Cdist))){ # if TRUE=0 then go to #NEXT#
        GPS <- GPS %>% drop_na(Cdist)
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
      
      print(table(is.na(GPS$Mspeed)))
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
      while(any(is.na(GPS$Cdist))){#Repeat until all deleted
        GPS <- GPS %>% drop_na(Cdist)
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
      print(table(is.na(GPS$Mspeed)))
      
      #SAVE GPS data as RData
      if(myseason <= 2017){
        if(ntrips == 1){
          saveRDS(GPS, file=paste0(datapath,"/Cleaned/GPS_", GPS$Stage[1], GPS$Nest[1], GPS$Sex[1], depID,".rds"))
        }
        else{
          saveRDS(GPS, file=paste0(datapath,"/Cleaned/GPS_", GPS$Stage[1], GPS$Nest[1], GPS$Sex[1], depID, "_", q, ".rds"))
        }
      }else{
        if(ntrips == 1){
          saveRDS(GPS, file=paste0(datapath,"/Cleaned/GPS_",depID,".rds"))
        }
        else{
          saveRDS(GPS, file=paste0(datapath,"/Cleaned/GPS_",depID,"_",q,".rds"))
=======
>>>>>>> 955f76970b1a130701cddc5e6b685d6e59902961
        }

      }
      
    }
  }else{print("GPS track not complete")}
}

my_files <- list.files("Data/LPPI2016/GPS/Cleaned/old", ".rds")

for (k in my_files) {
  my_gps <- readRDS(paste0("Data/LPPI2016/GPS/Cleaned/old/", k))
  saveRDS(my_gps, paste0("Data/LPPI2016/GPS/Cleaned/", gsub("8061M", "",k)))
}
