#~~~~~#
rm(list = ls())
set.seed(2498)
library(tidyverse);library(lubridate)
library(viridis);library(tools);library(rnaturalearth)
library(data.table);library(ggspatial)
#~~~~~#

# Step 1: Extract the depth from the axy data####
getmode <- function(v){# Function to get the mode of the pressure
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

acc_path <- "Data/LPPI2019/ACCcsv" # Path of acc files
my_acc <- tools::file_path_sans_ext(list.files(acc_path, pattern = ".csv")) # Get the acc filenames

my_global <- readRDS("Data/LPPI2019/Interpolated_GPS_2019.rds") # File of interpolated tracks
my_gps <- names(table(my_global$crwPredict$bird_ID)) # Extract the bird ID 

to_load <- intersect(my_acc, my_gps) # Acc files to load to extract the depth because we have a matching clean GPS track

# Update the gps list
my_global$crwPredict <- my_global$crwPredict[which(my_global$crwPredict$bird_ID %in% to_load),]
my_global$crwFits <- my_global$crwFits[which(names(my_global$crwFits) %in% names(table(my_global$crwPredict$ID)))]
saveRDS(my_global, paste0("Data/LPPI2019/Interpolated_GPS_2019.rds"))

for (i in 1:length(to_load)) {# Extract and save the depth
  axydata <- read.csv(paste0(acc_path,"/",to_load[i],".csv"))
  axydata <- axydata %>% dplyr::select(TagID, Timestamp, Pressure) %>% filter(!is.na(Pressure))
  mymode <- getmode(axydata$Pressure)
  axydata$Depth <- (axydata$Pressure-mymode)/100
  axydata <- axydata %>% dplyr::select(TagID, Timestamp, Depth)
  saveRDS(axydata, paste0("Data/LPPI2019/Depth/", to_load[i], ".rds"))
  print(paste0(i, "/", length(to_load)))
}

# Step 2: Correct the 0m value of the depth####
depth_path <- "Data/LPPI2019/Depth"
depth_files <- list.files(depth_path, pattern = ".rds")

for (j in 14:length(depth_files)) {
  print(depth_files[j])
  
  depth_data <- readRDS(paste0(depth_path,"/",depth_files[j]))
  
  depth_data$Timestamp <- with_tz(dmy_hms(depth_data$Timestamp, tz = "UTC"), tz="Australia/Melbourne")
  
  # Remove first and last value because of errors
  depth_data <- depth_data[c(-1, -length(depth_data)),]

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
                    ylim(NA, maxdepth)
  
  print(correct_plot)
  
  depth_data$dive <- ifelse(depth_data$correc_depth >= 2, 1, 0) # Dive is 2M
  
  saveRDS(depth_data, paste0(depth_path, "/corrected/", depth_files[j]))
  
  keep <- readline(prompt = "Continue? (y/n) ")
  if(keep == "n"){stop("Process stopped")}

}

# Step 3: Synch with the GPS####

depth_path <- "Data/LPPI2019/Depth/corrected"

my_depth <- tools::file_path_sans_ext(list.files(depth_path, pattern = ".rds")) # Get the depth filenames
my_global <- readRDS("Data/LPPI2019/Interpolated_GPS_2019.rds")
rownames(my_global$crwPredict) <- 1:nrow(my_global$crwPredict)

my_gps <- names(table(my_global$crwPredict$bird_ID)) # Extract the bird ID 

to_load <- intersect(my_depth, my_gps)
loop_index <- as.numeric(rownames(my_global$crwPredict[!duplicated(my_global$crwPredict$ID),]))

my_global$crwPredict$prop_dive <- NA
my_global$crwPredict$max_depth <- NA

j <- 1 # Loop index count

for (i in loop_index) {
  if(i != loop_index[length(loop_index)]){
    last_ping <- my_global$crwPredict[loop_index[j+1]-1,]
  }else{
    last_ping <- my_global$crwPredict[nrow(my_global$crwPredict),]
  }

  trackID <- my_global$crwPredict$bird_ID[i]
  mydive <- readRDS(paste0(depth_path,"/",trackID,".rds"))

  mymin <- i
  mingps <- which(mydive$Timestamp == my_global$crwPredict$dt[mymin])
  while (length(mingps) == 0) {
    mymin <- mymin + 1
    mingps <- which(mydive$Timestamp == my_global$crwPredict$dt[mymin])
  }
  
  maxgps <- which(mydive$Timestamp == last_ping$dt)
  filtered_dive <- mydive[mingps:maxgps,]
  
  for (k in (mymin+1):as.numeric(row.names(last_ping))) {
    start_dive <- my_global$crwPredict$dt[k-1]
    end_dive <- my_global$crwPredict$dt[k]
    
    the_dive <- filtered_dive %>% filter(Timestamp > start_dive & Timestamp <= end_dive)
    my_global$crwPredict$prop_dive[k] <- sum(the_dive$dive)/nrow(the_dive)
    my_global$crwPredict$max_depth[k] <- max(the_dive$correc_depth)
  }
  print(paste0(j,"/",length(loop_index)))
  j <- j+1
}

saveRDS(my_global, "Data/LPPI2019/Interpolated_GPS_Depth_2019.rds")

# (Optional: Plot)####
world <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")

# CRS string for Australia
ESPG <- x<-rgdal::make_EPSG()
myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
proj.aeqd <- myESPG$prj4

track_plot <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = mygps, aes(mu.x, mu.y, color = prop_dive)) +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1.3) +
  coord_sf(crs = proj.aeqd, expand = T, xlim = c(min(mygps$mu.x), max(mygps$mu.x)), ylim = c(min(mygps$mu.y), max(mygps$mu.y))) +
  labs(title = "Interpolated GPS track with dive information") +
  theme_bw() +
  theme(panel.grid = element_line(color = "#C4C4C4"))

speed_plot <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = mygps, aes(mu.x, mu.y, color = speed/1000)) +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1.3) +
  coord_sf(crs = proj.aeqd, expand = T, xlim = c(min(mygps$mu.x), max(mygps$mu.x)), ylim = c(min(mygps$mu.y), max(mygps$mu.y))) +
  labs(title = "Interpolated GPS track with dive information") +
  theme_bw() +
  scale_color_continuous(type = "viridis") +
  theme(panel.grid = element_line(color = "#C4C4C4"))

cowplot::plot_grid(track_plot, speed_plot)
