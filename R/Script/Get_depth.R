rm(list = ls())
set.seed(456)

library(tidyverse);library(lubridate);library(viridis)

#Step 1: Extract the depth from the axy data####
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

acc_path <- "Data/LPPI2019/ACCcsv"
gps_path <- "Data/LPPI2019/GPStxt/Cleaned"

my_acc <- tools::file_path_sans_ext(list.files(acc_path, pattern = ".csv")) #Get the acc filenames
my_gps <- tools::file_path_sans_ext(list.files(gps_path, pattern = ".rds")) #Get the GPS filenames
my_gps <- substr(my_gps,5,nchar(my_gps)) #Extract the track ID from GPS filenames

to_load <- intersect(my_acc, my_gps) #Acc files to load to extract the depth because we have a matching clean GPS track

for (i in 1:length(to_load)) {
  axydata <- read.csv(paste0(acc_path,"/",to_load[i],".csv"))
  axydata <- axydata %>% select(TagID, Timestamp, Pressure) %>% filter(!is.na(Pressure))
  mymode <- getmode(axydata$Pressure)
  axydata$Depth <- (axydata$Pressure-mymode)/100
  axydata <- axydata %>% select(TagID, Timestamp, Depth)
  saveRDS(axydata, paste0("Data/LPPI2019/Depth/", to_load[i], ".rds"))
  print(paste0(i, "/", length(to_load)))
}

#Step 2: Correct the 0m value of the depth####
depth_path <- "Data/LPPI2019/Depth"
depth_files <- list.files(depth_path, pattern = ".rds")

for (j in 18:length(depth_files)) {
  print(depth_files[j])
  
  depth_data <- readRDS(paste0(depth_path,"/",depth_files[j]))
  
  depth_data$Timestamp <- with_tz(dmy_hms(depth_data$Timestamp, tz = "UTC"), tz="Australia/Melbourne")
  
  #remove first and last value because of errors
  depth_data <- depth_data[c(-1, -length(depth_data)),]
  
  # ori_plot <- ggplot(depth_data, aes(Timestamp, Depth)) +
  #               geom_point() +
  #               ylim(NA,1)
  # 
  # plot(ori_plot)

  satisfaction <- "n"
  mintime <- NA
  maxtime <- NA
  maxdepth <- 0.1
  polydeg <- 20
  while (satisfaction != "y") {
    sub_depth_data <- depth_data %>% filter(Depth <= maxdepth)
    model <- lm(Depth ~ poly(Timestamp,polydeg), sub_depth_data)
    xseq <- depth_data$Timestamp
    pred <- predict(model, newdata = data.frame(Timestamp = xseq))
    depth_data$newzero <- pred
    
    if(is.na(maxtime) & is.na(mintime)){
      process_plot <- ggplot(depth_data) +
        geom_point(aes(Timestamp, Depth)) +
        ylim(NA, maxdepth) +
        geom_smooth(aes(Timestamp, newzero), stat = "identity")
    }else{
      process_plot <- ggplot(depth_data) +
        geom_point(aes(Timestamp, Depth)) +
        xlim(mintime, maxtime) +
        ylim(NA, maxdepth) +
        geom_smooth(aes(Timestamp, newzero), stat = "identity")
    }
    
    print(process_plot)
    Sys.sleep(2)
    
    satisfaction <- readline(prompt = "Is the 0m fit OK? (y/n) ")
    
    if(satisfaction == "n"){
      refinex <- readline(prompt = "Refine x scale? (y/n) ")
      if(refinex == "y"){
        mintime <- dmy_hms(readline(prompt = "Refine x scale: min timestamp? (dmy hms) "), tz="Australia/Melbourne")
        maxtime <- dmy_hms(readline(prompt = "Refine x scale: max timestamp? (dmy hms) "), tz="Australia/Melbourne")
      }
      maxdepth <- as.numeric(readline(prompt = "Refine y scale: max depth? "))
      polydeg <- as.numeric(readline(prompt = "Degree of the polynomial regression to fit? "))
    }
  }
  
  depth_data$correc_depth <- depth_data$Depth - depth_data$newzero
  
  correct_plot <- ggplot(depth_data) +
                    geom_point(aes(Timestamp, Depth)) +
                    geom_point(aes(Timestamp, correc_depth), color = viridis(1), alpha = 0.5) +
                    #xlim(mintime, maxtime) +
                    ylim(NA, maxdepth)
  
  print(correct_plot)
  
  depth_data$dive <- ifelse(depth_data$correc_depth >= 2, 1, 0) #Dive is 2M
  
  saveRDS(depth_data, paste0(depth_path, "/corrected/", depth_files[j]))
  
  keep <- readline(prompt = "Continue? (y/n) ")
  if(keep == "n"){stop("Process stopped")}

}

#Step 3: Synch with the GPS####
library(data.table);library(ggspatial)

mygps <- readRDS("Data/LPPI2019/GPStxt/Cleaned/Interpolated/GPS_G3013FP4.rds")
mydive <- readRDS("Data/LPPI2019/Depth/corrected/G3013FP4.rds")

mingps <- which(mydive$Timestamp == mygps$dt[1])
maxgps <- which(mydive$Timestamp == mygps$dt[length(mygps$dt)])
filtered_dive <- mydive[mingps:maxgps,]

mygps$prop_dive <- NA
mygps$max_depth <- NA

for(k in 2:nrow(mygps)){
  start_dive <- mygps$dt[k-1]
  end_dive <- mygps$dt[k]
  
  the_dive <- filtered_dive %>% filter(Timestamp > start_dive & Timestamp <= end_dive)
  mygps$prop_dive[k] <- sum(the_dive$dive)/nrow(the_dive)
  mygps$max_depth[k] <- max(the_dive$correc_depth)
}

world <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")

#CRS string for australia
ESPG <- x<-rgdal::make_EPSG()
myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
proj.aeqd <- myESPG$prj4

track_plot <- ggplot(data = world) +
  #geom_sf() +
  geom_path(data = mygps, aes(mu.x, mu.y)) +
  geom_point(data = mygps, aes(mu.x, mu.y, color = prop_dive)) +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1.3) +
  coord_sf(crs = proj.aeqd, expand = T, xlim = c(min(mygps$mu.x), max(mygps$mu.x)), ylim = c(min(mygps$mu.y), max(mygps$mu.y))) +
  labs(title = "Interpolated GPS track with dive information") +
  theme_bw() +
  theme(panel.grid = element_line(color = "#C4C4C4"))

track_plot





#NEED to join by DT and then to summarise to get the proportion of time spent diving in the interval
# dates$group <- cumsum(ifelse(difftime(dates$datecol,
#                                       shift(dates$datecol, fill = dates$datecol[1]), 
#                                       units = "days") >= 5 
#                              ,1, 0)) + 1