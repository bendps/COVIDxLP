#~~~~~#
rm(list = ls())
set.seed(2498)
library(tidyverse);library(lubridate)
library(sp);library(sf)
library(momentuHMM); library(ggspatial)
#~~~~~#

myseason <- 2014

# 1. Load####
# Merge all tracks
track_path <- paste0("Data/LPPI",myseason,"/GPS/Cleaned")
track_list <- list.files(track_path, ".rds")
myglobal <- data.frame()

for (i in 1:length(track_list)) {
  myGPS <- readRDS(paste0(track_path,"/", track_list[i]))
  myGPS$track_ID <- substr(track_list[i],5,nchar(track_list[i])-4) 
  myglobal <- rbind(myglobal, myGPS)
}

# Add bird ID
myglobal$bird_ID <- str_extract(myglobal$track_ID, "^.*(?=(_))")
myglobal$bird_ID[which(is.na(myglobal$bird_ID))] <- myglobal$track_ID[which(is.na(myglobal$bird_ID))]

# For 2017/2016
if(myseason == 2017 | myseason == 2016){
  for (k in 1:nrow(myglobal)) {
    if(grepl("_",substr(myglobal$track_ID[k], nchar(myglobal$track_ID[k])-1, nchar(myglobal$track_ID[k]))) == FALSE){
      myglobal$bird_ID[k] <- myglobal$track_ID[k]
    }
  }
  myglobal$bird_ID[!which(!grepl("_",substr(myglobal$track_ID, nchar(myglobal$track_ID)-1, nchar(myglobal$track_ID))))] <- myglobal$track_ID
  
}

# Interpolate
lpproj <- myglobal

#2. Project the original data####
# Prepare CRS Strings
# CRS for WGS84
proj.latlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# CRS string for Australia
ESPG <- x<-rgdal::make_EPSG()
myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
proj.aeqd <- myESPG$prj4

# Transform to spatial object 
coordinates(lpproj) <- ~ Longitude + Latitude

# Define CRS
proj4string(lpproj) <- CRS(proj.latlon)

# Project the data
lpproj <- spTransform(lpproj, CRS(proj.aeqd))

#3. SPatial interpolation####
# Prepare for momentuHMM
obstimes <- as.numeric(lpproj$Dt) / 3600 # convert time to numeric hours
lnError <- crawl::argosDiag2Cov(50,50,0) # assume 50m isotropic error ellipse
lpdata <- data.frame(ID=lpproj$track_ID,
                     bird_ID = lpproj$bird_ID,
                     time=obstimes,
                     x=lpproj@coords[, 1],
                     y=lpproj@coords[, 2],
                     ln.sd.x=lnError$ln.sd.x, 
                     ln.sd.y = lnError$ln.sd.y, 
                     error.corr= lnError$error.corr)

# MomentuHMM Spatial interpolation
crwOut <- crawlWrap(lpdata,theta=c(6.5,-.1),fixPar=c(1,1,NA,NA),
                    err.model = list(x=~ln.sd.x-1,y=~ln.sd.y-1,rho=~error.corr),
                    timeStep=0.25, # predict at 15 min time steps
                    attempts=10)

crwOut$crwPredict <- crwOut$crwPredict[which(crwOut$crwPredict$locType == "p"),] # Keep only interpolated data

crwOut$crwPredict$dt <- with_tz(as_datetime(crwOut$crwPredict$time * 3600), tz = "Australia/Melbourne")

for(k in 1:nrow(crwOut$crwPredict)){
  if(is.na(crwOut$crwPredict$bird_ID[k])){
    crwOut$crwPredict$bird_ID[k] <- crwOut$crwPredict$bird_ID[k-1]
  }
}



saveRDS(crwOut, paste0("Data/LPPI",myseason,"/Interpolated_GPS_",myseason,".rds"))

plot(crwOut, compact = T)


