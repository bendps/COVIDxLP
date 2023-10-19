#~~~~~#
rm(list = ls())
#set.seed(2498)
library(sp);library(scales);library(raster)
library(viridis);library(proj4);library(tidyverse)
library(lubridate);library(suncalc)
#~~~~~#

#Input parameters
mult_seasons <- 2010:2020 #What years do you want to clean?

for(myseason in mult_seasons){
  print(myseason)
  if(myseason != 2016){
    if(myseason %in% c(2020, 2018)){
      toremove <- 7 # Characters to remove from file names to get the full ID
    }else if(myseason %in% c(2014)){
      toremove <- 14
    }else if(myseason %in% c(2013)){
      toremove <- 15
    }else if(myseason %in% c(2012)){
      toremove <- 16
    }else{toremove <- 4}
    
    
    datapath <- paste0("Data/LPPI",myseason,"/GPS") #Path to the GPS files
    deppath <- paste0("Data/LPPI",myseason,"/Deployment_LP_",myseason,".csv") # Path to the deployment .csv file
    
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
    if(myseason == 2012){
      deployment <- deployment %>% filter(!is.na(GPS.file.name))
    }
    
    weighbridge <- readRDS("Data/weighbridge.rds")
    
    for(n in 1:length(flist)){
      print(paste0(n,"/", length(flist)))
      #Get deployment and weighbridge data
      if(myseason == 2015){
        depID <- substr(flist[n], 1, nchar(flist[n])-4)
        mypit <- substr(flist[n], 6, 11)
        print(depID)
        mydeploy <- tibble(clutch_number = 1)
        mybridge <- weighbridge %>% filter(pit_tag == mypit & season == myseason)
      }else if(myseason == 2017){
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
      if(mydeploy$clutch_number[1] != 2 & myseason > 2017){
        GPS <- read.table(paste0(datapath,"/",as.character(flist[n])))
      }else if(mydeploy$clutch_number[1] != 2 & myseason == 2017){
        GPS <- read.csv(paste0(datapath,"/",as.character(flist[n])), sep = ",", skip = 6)
      }else if(myseason <= 2015){
        GPS <- read.csv(paste0(datapath,"/",as.character(flist[n])), sep = ",")
      }
      
      if(mydeploy$clutch_number[1] != 2){
        #Name the columns & add infos
        if(myseason == 2010 | myseason == 2011 | myseason == 2012 | myseason == 2013 | myseason == 2014){
          names(GPS) <- c("Date", "Time", "Latitude","Longitude","Altitude", "Speed","Sat","Hdop","Sig", "TFFs")
          GPS$Dt <- paste(GPS$Date, GPS$Time)
          GPS <- GPS[,c(3:11)]
          GPS <- GPS %>% relocate(Dt, .before = Latitude)
          GPS <- GPS %>% dplyr::select(-TFFs)
          if(myseason == 2010 & n == 8){
            GPS$Dt <- dmy_hms(GPS$Dt, tz="Australia/Melbourne") #because some tracks are written with french type dates
          }else{GPS$Dt <- ymd_hms(GPS$Dt, tz="Australia/Melbourne")}
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
          GPS <- GPS %>% relocate(Speed, .before = Sat)
          GPS <- GPS %>% dplyr::select(-TFFs)
          GPS$Dt <- with_tz(mdy_hms(GPS$Dt, tz="UTC"), tz="Australia/Melbourne")
          GPS$Datef <- date(GPS$Dt) #or as.Date for UTC date
          GPS$Stage <- str_replace_all(str_extract(flist[n], "\\D+_"),"_", "")
          GPS$Nest <- str_replace_all(str_extract_all(flist, "_\\s*(.*?)\\s*_")[[n]][1], "_", "")
          if(str_detect(flist[n], "F") == TRUE){
            GPS$Sex <- "F"
          }else{GPS$Sex <- "M"}
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
                                      # keep = c("sunrise", "sunset"),
                                      keep = c("sunrise", "sunset","nauticalDawn", "nauticalDusk"),
                                      tz = "Australia/Melbourne")
        
        #Plots to check the data
        par(mfrow=c(2,2))
        plot(GPS$Dt,col=topo.colors(nrow(GPS)))
        plot(GPS$Longitude,GPS$Latitude,type="l")
        points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
        points(colon, colat, pch = 10)
        plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
        abline(v=sun_times$nauticalDawn, col = "blue")
        abline(v=sun_times$nauticalDusk, col = "red")
        abline(v=sun_times$sunrise, col = "purple")
        abline(v=sun_times$sunset, col = "pink")
        plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
        
        OriGPS <- GPS
        
        checktrack <- "y" #readline(prompt = "Clean this track? (y/n) ")
        if(checktrack == "y" & mean(GPS$Cdist) > 1000){ 
          #Remove manually abnormal positions
          abnormal <- "n" #readline(prompt = "Abnormal positions? (y/n) ")
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
              #next
              GPS<-GPS
              abnormal <- "n"}
            
            #Plots to check the data
            par(mfrow=c(2,2))
            plot(GPS$Dt,col=topo.colors(nrow(GPS)))
            plot(GPS$Longitude,GPS$Latitude,type="l")
            points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
            plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
            abline(v=sun_times$nauticalDawn, col = "blue")
            abline(v=sun_times$nauticalDusk, col = "red")
            abline(v=sun_times$sunrise, col = "purple")
            abline(v=sun_times$sunset, col = "pink")
            plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
            
            abnormal <- readline(prompt = "Abnormal positions? (y/n) ")
          }
          
          #Remove nights at nautical dawn/dusk
          colnames(sun_times)[1]<-"Datef"
          sun_times$DataChunk<-seq(1,nrow(sun_times),1)
          GPS <- inner_join(GPS, sun_times[,c("Datef","sunrise","sunset","nauticalDawn","nauticalDusk","DataChunk")])
          GPS <- GPS[which(GPS$Dt > GPS$nauticalDawn & GPS$Dt < GPS$nauticalDusk),]
          rownames(GPS) <- 1:nrow(GPS)
          
          #Remove false chunk when staying at colony (<1000m)
          chunk_dist <- GPS %>% group_by(DataChunk) %>% summarise(max_dist_chunk = max(Cdist, na.rm = T))
          chunk_removed <- which(chunk_dist$max_dist_chunk < 1000)
          if(length(chunk_removed > 0)){
            for (j in chunk_removed) {
              GPS <- GPS %>% filter(DataChunk != j)
            }
          }
          
          #Remove if chunk is less than 2 GPS point
          short_chunk <- as.vector(which(table(GPS$DataChunk) <= 2))
          if(length(short_chunk > 0)){
            for (pp in short_chunk) {
              GPS <- GPS %>% filter(DataChunk != pp)
            }
          }
          
          #Re-index chunks if necessary
          GPS$DataChunk <- rep(1:length(unique(GPS$DataChunk)), times = as.vector(table(GPS$DataChunk)))
          OriGPS <- GPS
          
          #Plots to check the data
          par(mfrow=c(2,2))
          plot(GPS$Dt,col=topo.colors(nrow(GPS)))
          plot(GPS$Longitude,GPS$Latitude,type="l")
          points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
          points(colon, colat, pch = 10)
          plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
          abline(v=sun_times$nauticalDawn, col = "blue")
          abline(v=sun_times$nauticalDusk, col = "red")
          abline(v=sun_times$sunrise, col = "purple")
          abline(v=sun_times$sunset, col = "pink")
          plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
          
          ntrips <- length(unique(GPS$DataChunk))
          if (ntrips > 1){
            print("You have multiple trips/chunks")
          }
          
          for (q in unique(GPS$DataChunk)) {
            print(paste0("Trip n°", q, " out of ", ntrips))
            subGPS <- GPS[which(GPS$DataChunk==q),]
            
            #Plots to check the data
            par(mfrow=c(2,2))
            plot(subGPS$Dt,col=topo.colors(nrow(subGPS)))
            plot(subGPS$Longitude,subGPS$Latitude,type="l")
            points(subGPS$Longitude,subGPS$Latitude,col=topo.colors(nrow(subGPS)))
            points(colon, colat, pch=20)
            plot(subGPS$Cdist~subGPS$Dt,col=topo.colors(nrow(subGPS)))
            abline(v=subGPS$nauticalDawn, col = "blue")
            abline(v=subGPS$nauticalDusk, col = "red")
            plot(subGPS$Cdist,col=topo.colors(nrow(subGPS)))
            
            checktrack <- "y" #readline(prompt = "Keep this chunk? (y/n) ")
            if(checktrack == "n"){next}
            if(checktrack == "y"){
              
              par(mfrow=c(2,2))
              #Distance between points (m)
              subGPS$Dist <- rep(as.numeric(0), nrow(subGPS))
              for (i in 1:(nrow(subGPS)-1)) {
                subGPS$Dist[i] <- pointDistance(c(subGPS$Longitude[i], subGPS$Latitude[i]), c(subGPS$Longitude[i+1], subGPS$Latitude[i+1]),lonlat=T)  
              }
              plot(subGPS$Dt,subGPS$Dist)
              
              #Time between two points (hours)
              subGPS$Int <- rep(as.numeric(0), nrow(subGPS))
              for (i in 1:(nrow(subGPS)-1)) {
                subGPS$Int[i] <- as.numeric(difftime(subGPS$Dt[i+1], subGPS$Dt[i], tz='GMT', units='hours'))
              }
              plot(subGPS$Dt,subGPS$Int)
              
              #Speed between two points (meters/hours) 
              subGPS$Mspeed <- rep(as.numeric(0), nrow(subGPS))
              subGPS$Mspeed <- subGPS$Dist/subGPS$Int
              subGPS$Mspeed[nrow(subGPS)] <- 0
              plot(subGPS$Dt,subGPS$Mspeed,col=topo.colors(nrow(subGPS)))
              plot(subGPS$Longitude, subGPS$Latitude, type="o", col=topo.colors(nrow(subGPS)))
              
              #CLEANING: find and remove some points >7 000 m/h & Int <0.002 h (7.2s)
              subGPS$Cdist[(subGPS$Int<0.002)&(subGPS$Mspeed>7000)] <- NA
              subGPS <- subGPS %>% drop_na(Int, Mspeed)
              
              #Re-cal Mspeed
              rownames(subGPS)<-1:nrow(subGPS)
              for (i in 1:(nrow(subGPS)-1)) {
                subGPS$Dist[i] <- pointDistance(c(subGPS$Longitude[i], subGPS$Latitude[i]), c(subGPS$Longitude[i+1], subGPS$Latitude[i+1]),lonlat=T)
                subGPS$Int[i] <- as.numeric(difftime(subGPS$Dt[i+1], subGPS$Dt[i], tz='GMT', units='hours'))
              }
              subGPS$Mspeed <- subGPS$Dist/subGPS$Int
              subGPS$Mspeed[nrow(subGPS)] <- 0
              
              #Find and remove second points >8 000 m/h
              par(mfrow=c(1,1))
              plot(subGPS$Dt,subGPS$Mspeed)
              for (i in 2:(nrow(subGPS))) {
                if(subGPS$Mspeed[i-1]>8000 & subGPS$Mspeed[i]>8000) {
                  subGPS$Cdist[i] <- NA
                  subGPS$Mspeed[i] <- 0}
              }
              while(any(is.na(subGPS$Cdist))){ # if TRUE=0 then go to #NEXT#
                subGPS <- subGPS %>% drop_na(Cdist)
                if(nrow(subGPS > 2)){
                  #re-cal Mspeed
                  rownames(subGPS)<-1:nrow(subGPS)
                  for (i in 1:(nrow(subGPS)-1)) {
                    subGPS$Dist[i] <- pointDistance(c(subGPS$Longitude[i], subGPS$Latitude[i]), c(subGPS$Longitude[i+1], subGPS$Latitude[i+1]),lonlat=T)
                    subGPS$Int[i] <- as.numeric(difftime(subGPS$Dt[i+1], subGPS$Dt[i], tz='GMT', units='hours'))
                  }
                  subGPS$Mspeed <- subGPS$Dist/subGPS$Int
                  subGPS$Mspeed[nrow(subGPS)] <- 0
                  for (i in 2:(nrow(subGPS))) {
                    if(subGPS$Mspeed[i-1]>8000 & subGPS$Mspeed[i]>8000) {
                      subGPS$Cdist[i] <- NA
                      subGPS$Mspeed[i] <- 0}
                  }
                }
              }
              
              print(table(is.na(subGPS$Mspeed)))
              plot(subGPS$Dt,subGPS$Mspeed)
              
              #Remove false chunk when staying at colony (<1000m)
              sub_chunk <- subGPS %>% summarise(max_dist_chunk = max(Cdist, na.rm = T))
              chunk_col <- sub_chunk$max_dist_chunk[1] < 1000
              
              #Remove if chunk is less than 2 GPS point
              chunk_short <- nrow(subGPS) <= 2 
              
              if(chunk_col == T | chunk_short == T){
                print("chunk discarded p1")
              }else{
                #NEXT# find and remove single points >8 000 m/h
                for (i in 2:(nrow(subGPS)-1)) {
                  if(subGPS$Mspeed[i]>8000) { 
                    if(subGPS$Mspeed[i-1]>subGPS$Mspeed[i+1]) {
                      subGPS$Cdist[i] <- NA}
                    else{
                      subGPS$Cdist[i+1] <- NA}
                  }
                }
                while(any(is.na(subGPS$Cdist))){#Repeat until all deleted
                  subGPS <- subGPS %>% drop_na(Cdist)
                  if(nrow(subGPS) > 2){
                    #Re-cal Mspeed
                    rownames(subGPS)<-1:nrow(subGPS)
                    for (i in 1:(nrow(subGPS)-1)) {
                      subGPS$Dist[i] <- pointDistance(c(subGPS$Longitude[i], subGPS$Latitude[i]), c(subGPS$Longitude[i+1], subGPS$Latitude[i+1]),lonlat=T)
                      subGPS$Int[i] <- as.numeric(difftime(subGPS$Dt[i+1], subGPS$Dt[i], tz='GMT', units='hours'))
                    }
                    subGPS$Mspeed <- subGPS$Dist/subGPS$Int
                    subGPS$Mspeed[nrow(subGPS)] <- 0
                    #Remove points
                    for (i in 2:(nrow(subGPS)-1)) {
                      if(subGPS$Mspeed[i]>8000) { 
                        if(subGPS$Mspeed[i-1]>subGPS$Mspeed[i+1]) {
                          subGPS$Cdist[i] <- NA}
                        else{
                          subGPS$Cdist[i+1] <- NA}
                      }
                    }
                  }
                }
                
                plot(subGPS$Dt,subGPS$Mspeed)
                print(table(is.na(subGPS$Mspeed)))
                
                #print length of dataset 
                lengthTrip<-difftime(subGPS$Dt[nrow(subGPS)], subGPS$Dt[1], units = "hours")
                print(lengthTrip)
              }
              
              #Remove false chunk when staying at colony (<1000m)
              sub_chunk <- subGPS %>% summarise(max_dist_chunk = max(Cdist, na.rm = T))
              chunk_col <- sub_chunk$max_dist_chunk[1] < 1000
              
              #Remove if chunk is less than 2 GPS point
              chunk_short <- nrow(subGPS) <= 2 
              
              
              #SAVE GPS data as RData
              if(chunk_col == T | chunk_short == T){
                print("chunk discarded p2")
              }else{
                if(myseason <= 2017 & myseason > 2014 & myseason != 2015){
                  if(ntrips == 1){
                    saveRDS(subGPS, file=paste0(datapath,"/Cleaned/GPS_", subGPS$Stage[1], subGPS$Nest[1], subGPS$Sex[1], depID,".rds"))
                  }
                  else{
                    saveRDS(subGPS, file=paste0(datapath,"/Cleaned/GPS_", subGPS$Stage[1], subGPS$Nest[1], subGPS$Sex[1], depID, "_", q, ".rds"))
                  }
                }else{
                  if(ntrips == 1){
                    saveRDS(subGPS, file=paste0(datapath,"/Cleaned/GPS_",depID,".rds"))
                  }
                  else{
                    saveRDS(subGPS, file=paste0(datapath,"/Cleaned/GPS_",depID,"_",q,".rds"))
                  }
                }
              }
            } 
          }
        }
      }
      else{print("GPS track not complete")}
    }
  }else if(myseason == 2016){
    datapath <- paste0("Data/LPPI",myseason,"/GPS") #Path to the GPS files
    weighbridge <- readRDS("Data/weighbridge.rds")
    lp_gps <- readRDS("Data/LPPI2016/GPS/lp_GPS_2016.rds")
    
    lp_gps <- lp_gps %>% filter(deployment_site == "Parade", clutch_no == 1) # filter the data to keep only pinp and first clutches
    rownames(lp_gps) <- 1:nrow(lp_gps)
    lp_gps$index <- as.numeric(rownames(lp_gps))
    loop_index <- lp_gps$index[match(unique(lp_gps$input_file), lp_gps$input_file)] # get the first occurence of the tracks
    #Correct stage annotation
    mydeploy <- read.csv("Data/LPPI2016/Deployment_LPPI_2016.csv", sep = ";")
    
    for (i in loop_index) {
      # reshape the gps file
      GPS <- lp_gps %>% filter(input_file == lp_gps$input_file[i])
      
      depID <- str_match(GPS$input_file[1], "/\\s*(.*?)\\s*.csv")[,2] #get dep_id for Axy data
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
                                    # keep = c("sunrise", "sunset"),
                                    keep = c("sunrise", "sunset","nauticalDawn", "nauticalDusk"),
                                    tz = "Australia/Melbourne")
      
      #Plots to check the data
      par(mfrow=c(2,2))
      plot(GPS$Dt,col=topo.colors(nrow(GPS)))
      plot(GPS$Longitude,GPS$Latitude,type="l")
      points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
      points(colon, colat, pch = 10)
      plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
      abline(v=sun_times$nauticalDawn, col = "blue")
      abline(v=sun_times$nauticalDusk, col = "red")
      abline(v=sun_times$sunrise, col = "purple")
      abline(v=sun_times$sunset, col = "pink")
      plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
      
      OriGPS <- GPS
      
      checktrack <- "y" #readline(prompt = "Clean this track? (y/n) ")
      if(checktrack == "y"){ 
        #Remove manually abnormal positions
        abnormal <- "n" #readline(prompt = "Abnormal positions? (y/n) ")
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
            #next
            GPS<-GPS
            abnormal <- "n"}
          
          #Plots to check the data
          par(mfrow=c(2,2))
          plot(GPS$Dt,col=topo.colors(nrow(GPS)))
          plot(GPS$Longitude,GPS$Latitude,type="l")
          points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
          plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
          abline(v=sun_times$nauticalDawn, col = "blue")
          abline(v=sun_times$nauticalDusk, col = "red")
          abline(v=sun_times$sunrise, col = "purple")
          abline(v=sun_times$sunset, col = "pink")
          plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
          
          abnormal <- readline(prompt = "Abnormal positions? (y/n) ")
        }
        
        #Remove nights at nautical dawn/dusk
        colnames(sun_times)[1]<-"Datef"
        sun_times$DataChunk<-seq(1,nrow(sun_times),1)
        GPS <- inner_join(GPS, sun_times[,c("Datef","sunrise","sunset","nauticalDawn","nauticalDusk","DataChunk")])
        GPS <- GPS[which(GPS$Dt > GPS$nauticalDawn & GPS$Dt < GPS$nauticalDusk),]
        rownames(GPS) <- 1:nrow(GPS)
        
        #Plots to check the data
        par(mfrow=c(2,2))
        plot(GPS$Dt,col=topo.colors(nrow(GPS)))
        plot(GPS$Longitude,GPS$Latitude,type="l")
        points(GPS$Longitude,GPS$Latitude,col=topo.colors(nrow(GPS)))
        points(colon, colat, pch = 10)
        plot(GPS$Cdist~GPS$Dt,col=topo.colors(nrow(GPS)))
        abline(v=sun_times$nauticalDawn, col = "blue")
        abline(v=sun_times$nauticalDusk, col = "red")
        abline(v=sun_times$sunrise, col = "purple")
        abline(v=sun_times$sunset, col = "pink")
        plot(GPS$Cdist,col=topo.colors(nrow(GPS)))
        
        #Remove false chunk when staying at colony (<1000m)
        chunk_dist <- GPS %>% group_by(DataChunk) %>% summarise(max_dist_chunk = max(Cdist, na.rm = T))
        chunk_removed <- which(chunk_dist$max_dist_chunk < 1000)
        if(length(chunk_removed > 0)){
          for (c in chunk_removed) {
            GPS <- GPS %>% filter(DataChunk != chunk_removed)
          }
        }
        
        #Remove if chunk is less than 2 GPS point
        short_chunk <- as.vector(which(table(GPS$DataChunk) <= 2))
        if(length(short_chunk > 0)){
          for (c in short_chunk) {
            GPS <- GPS %>% filter(DataChunk != short_chunk)
          }
        }
        
        #Re-index chunks if necessary
        GPS$DataChunk <- rep(1:length(unique(GPS$DataChunk)), times = as.vector(table(GPS$DataChunk)))
        OriGPS <- GPS
        
        ntrips <- length(unique(GPS$DataChunk))
        if (ntrips > 1){
          print("You have multiple trips/chunks")
        }
        
        for (q in unique(GPS$DataChunk)) {
          
          print(paste0("Trip n°", q, " out of ", ntrips))
          subGPS <- GPS[which(GPS$DataChunk==q),]
          
          #Plots to check the data
          par(mfrow=c(2,2))
          plot(subGPS$Dt,col=topo.colors(nrow(subGPS)))
          plot(subGPS$Longitude,subGPS$Latitude,type="l")
          points(subGPS$Longitude,subGPS$Latitude,col=topo.colors(nrow(subGPS)))
          points(colon, colat, pch=20)
          plot(subGPS$Cdist~subGPS$Dt,col=topo.colors(nrow(subGPS)))
          abline(v=subGPS$nauticalDawn, col = "blue")
          abline(v=subGPS$nauticalDusk, col = "red")
          plot(subGPS$Cdist,col=topo.colors(nrow(subGPS)))
          
          checktrack <- "y" #readline(prompt = "Keep this chunk? (y/n) ")
          if(checktrack == "n"){next}
          if(checktrack == "y"){
            par(mfrow=c(2,2))
            #Distance between points (m)
            subGPS$Dist <- rep(as.numeric(0), nrow(subGPS))
            for (i in 1:(nrow(subGPS)-1)) {
              subGPS$Dist[i] <- pointDistance(c(subGPS$Longitude[i], subGPS$Latitude[i]), c(subGPS$Longitude[i+1], subGPS$Latitude[i+1]),lonlat=T)  
            }
            plot(subGPS$Dt,subGPS$Dist)
            
            #Time between two points (hours)
            subGPS$Int <- rep(as.numeric(0), nrow(subGPS))
            for (i in 1:(nrow(subGPS)-1)) {
              subGPS$Int[i] <- as.numeric(difftime(subGPS$Dt[i+1], subGPS$Dt[i], tz='GMT', units='hours'))
            }
            plot(subGPS$Dt,subGPS$Int)
            
            #Speed between two points (meters/hours) 
            subGPS$Mspeed <- rep(as.numeric(0), nrow(subGPS))
            subGPS$Mspeed <- subGPS$Dist/subGPS$Int
            subGPS$Mspeed[nrow(subGPS)] <- 0
            plot(subGPS$Dt,subGPS$Mspeed,col=topo.colors(nrow(subGPS)))
            plot(subGPS$Longitude, subGPS$Latitude, type="o", col=topo.colors(nrow(subGPS)))
            
            #CLEANING: find and remove some points >7 000 m/h & Int <0.002 h (7.2s)
            subGPS$Cdist[(subGPS$Int<0.002)&(subGPS$Mspeed>7000)] <- NA
            subGPS <- subGPS %>% drop_na(Int, Mspeed)
            
            #Re-cal Mspeed
            rownames(subGPS)<-1:nrow(subGPS)
            for (i in 1:(nrow(subGPS)-1)) {
              subGPS$Dist[i] <- pointDistance(c(subGPS$Longitude[i], subGPS$Latitude[i]), c(subGPS$Longitude[i+1], subGPS$Latitude[i+1]),lonlat=T)
              subGPS$Int[i] <- as.numeric(difftime(subGPS$Dt[i+1], subGPS$Dt[i], tz='GMT', units='hours'))
            }
            subGPS$Mspeed <- subGPS$Dist/subGPS$Int
            subGPS$Mspeed[nrow(subGPS)] <- 0
            
            #Find and remove second points >8 000 m/h
            par(mfrow=c(1,1))
            plot(subGPS$Dt,subGPS$Mspeed)
            for (i in 2:(nrow(subGPS))) {
              if(subGPS$Mspeed[i-1]>8000 & subGPS$Mspeed[i]>8000) {
                subGPS$Cdist[i] <- NA
                subGPS$Mspeed[i] <- 0}
            }
            while(any(is.na(subGPS$Cdist))){ # if TRUE=0 then go to #NEXT#
              subGPS <- subGPS %>% drop_na(Cdist)
              #re-cal Mspeed
              rownames(subGPS)<-1:nrow(subGPS)
              for (i in 1:(nrow(subGPS)-1)) {
                subGPS$Dist[i] <- pointDistance(c(subGPS$Longitude[i], subGPS$Latitude[i]), c(subGPS$Longitude[i+1], subGPS$Latitude[i+1]),lonlat=T)
                subGPS$Int[i] <- as.numeric(difftime(subGPS$Dt[i+1], subGPS$Dt[i], tz='GMT', units='hours'))
              }
              subGPS$Mspeed <- subGPS$Dist/subGPS$Int
              subGPS$Mspeed[nrow(subGPS)] <- 0
              for (i in 2:(nrow(subGPS))) {
                if(subGPS$Mspeed[i-1]>8000 & subGPS$Mspeed[i]>8000) {
                  subGPS$Cdist[i] <- NA
                  subGPS$Mspeed[i] <- 0}
              }
            }
            
            print(table(is.na(subGPS$Mspeed)))
            plot(subGPS$Dt,subGPS$Mspeed)
            
            #NEXT# find and remove single points >8 000 m/h
            for (i in 2:(nrow(subGPS)-1)) {
              if(subGPS$Mspeed[i]>8000) { 
                if(subGPS$Mspeed[i-1]>subGPS$Mspeed[i+1]) {
                  subGPS$Cdist[i] <- NA}
                else{
                  subGPS$Cdist[i+1] <- NA}
              }
            }
            while(any(is.na(subGPS$Cdist))){#Repeat until all deleted
              subGPS <- subGPS %>% drop_na(Cdist)
              #Re-cal Mspeed
              rownames(subGPS)<-1:nrow(subGPS)
              for (i in 1:(nrow(subGPS)-1)) {
                subGPS$Dist[i] <- pointDistance(c(subGPS$Longitude[i], subGPS$Latitude[i]), c(subGPS$Longitude[i+1], subGPS$Latitude[i+1]),lonlat=T)
                subGPS$Int[i] <- as.numeric(difftime(subGPS$Dt[i+1], subGPS$Dt[i], tz='GMT', units='hours'))
              }
              subGPS$Mspeed <- subGPS$Dist/subGPS$Int
              subGPS$Mspeed[nrow(subGPS)] <- 0
              #Remove points
              for (i in 2:(nrow(subGPS)-1)) {
                if(subGPS$Mspeed[i]>8000) { 
                  if(subGPS$Mspeed[i-1]>subGPS$Mspeed[i+1]) {
                    subGPS$Cdist[i] <- NA}
                  else{
                    subGPS$Cdist[i+1] <- NA}
                }
              }
            }
            
            plot(subGPS$Dt,subGPS$Mspeed)
            print(table(is.na(subGPS$Mspeed)))
            
            #print length of dataset 
            lengthTrip<-difftime(subGPS$Dt[nrow(subGPS)], subGPS$Dt[1], units = "hours")
            print(lengthTrip)
            
            subGPS$Stage[which(subGPS$Stage == "P")] <- "PG" #correct stage annotation
            #SAVE GPS data as RData
            
            if(ntrips == 1){
              saveRDS(subGPS, file=paste0(datapath,"/Cleaned/GPS_",subGPS$Stage[1],depID,".rds"))
            }
            else{
              saveRDS(subGPS, file=paste0(datapath,"/Cleaned/GPS_",subGPS$Stage[1],depID,"_",q,".rds"))
            }
            
          } 
        }
      }else{print("GPS track not complete")}
    }
  }
}

# Correct files names for 2017 
mytracks <- list.files("Data/LPPI2017/GPS/Cleaned", ".rds")

acc_path <- "Data/LPPI2017/ACCcsv" # Path of acc files
my_acc <- tools::file_path_sans_ext(list.files(acc_path, pattern = ".csv")) # Get the acc filenames

for (n in 1:length(mytracks)) {
  reptrip <- str_match(mytracks[n], "_([0-9]+).rds")[,2]
  if(is.na(reptrip)){
    mygps <- readRDS(paste0("Data/LPPI2017/GPS/Cleaned/", mytracks[n]))
    
    pitag <- substr(mytracks[n], nchar(mytracks[n])-9, nchar(mytracks[n])-4)
    
    oriname <- list.files("Data/LPPI2017/GPS", ".csv")[which(grepl(paste0(mygps$Nest[1],"_",mygps$Sex[1], "_", pitag),
                                                                   list.files("Data/LPPI2017/GPS", ".csv")))]
    oriname <- str_extract_all(oriname, "_2017\\s*(.*?)\\s*.csv")[[1]]
    lastdate <- substr(oriname, 2, nchar(oriname)-4)
    
    saveRDS(mygps,paste0("Data/LPPI2017/GPS/Cleaned/Renamed/GPS_",mygps$Nest[1],"_",mygps$Sex[1], "_", pitag, "_",lastdate,".rds"))
  }else{
    mygps <- readRDS(paste0("Data/LPPI2017/GPS/Cleaned/", mytracks[n]))
    
    fulltrip <- which(grepl(substr(mytracks[n], 1, nchar(mytracks[n])-6), mytracks))
    lastdate <- numeric()
    for (k in fulltrip) {
      alttrip <- readRDS(paste0("Data/LPPI2017/GPS/Cleaned/", mytracks[k]))
      lastdate <- c(lastdate,as.numeric(gsub("-","",as.Date(alttrip$Dt[nrow(alttrip)]))))
    }
    
    pitag <- substr(mytracks[n], nchar(mytracks[n])-11, nchar(mytracks[n])-6)
    
    oriname <- list.files("Data/LPPI2017/GPS", ".csv")[which(grepl(paste0(mygps$Nest[1],"_",mygps$Sex[1], "_", pitag),
                                                                   list.files("Data/LPPI2017/GPS", ".csv")))]
    oriname <- str_extract_all(oriname, "_2017\\s*(.*?)\\s*.csv")[[1]]
    lastdate <- substr(oriname, 2, nchar(oriname)-4)
    
    saveRDS(mygps,paste0("Data/LPPI2017/GPS/Cleaned/Renamed/GPS_",mygps$Nest[1],"_",mygps$Sex[1], "_", pitag, "_",lastdate,"_", reptrip,".rds"))
  }
  print(paste0(n,"/", length(mytracks)))
}


