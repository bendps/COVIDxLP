#~~~~~#
rm(list = ls())
set.seed(2498)
library(tidyverse);library(lubridate);library(tools)
#~~~~~#

mytracks <- list.files("Data/LPPI2017/GPS/Cleaned", ".rds")

acc_path <- "Data/LPPI2017/ACCcsv" # Path of acc files
my_acc <- tools::file_path_sans_ext(list.files(acc_path, pattern = ".csv")) # Get the acc filenames

for (n in 1:length(mytracks)) {
  reptrip <- str_match(mytracks[n], "_([0-9]+).rds")[,2]
  if(is.na(reptrip)){
    mygps <- readRDS(paste0("Data/LPPI2017/GPS/Cleaned/", mytracks[n]))
    
    pitag <- substr(mytracks[n], nchar(mytracks[n])-9, nchar(mytracks[n])-4)
    
    #lastdate <- gsub("-","",as.Date(mygps$Dt[nrow(mygps)]))
    
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
    
    # lastdate <- as.character(max(lastdate))
    
    oriname <- list.files("Data/LPPI2017/GPS", ".csv")[which(grepl(paste0(mygps$Nest[1],"_",mygps$Sex[1], "_", pitag),
                                                                        list.files("Data/LPPI2017/GPS", ".csv")))]
    oriname <- str_extract_all(oriname, "_2017\\s*(.*?)\\s*.csv")[[1]]
    lastdate <- substr(oriname, 2, nchar(oriname)-4)

    saveRDS(mygps,paste0("Data/LPPI2017/GPS/Cleaned/Renamed/GPS_",mygps$Nest[1],"_",mygps$Sex[1], "_", pitag, "_",lastdate,"_", reptrip,".rds"))
  }
  print(paste0(n,"/", length(mytracks)))
}
