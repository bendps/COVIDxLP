rm(list = ls())
set.seed(2498)

library(tidyverse);library(lubridate);library(sp);library(sf)
library(adehabitatLT);library(momentuHMM); library(ggspatial)


#1. Load####
mysample <- list.files("Data/LPPI2019/GPStxt/Cleaned", full.names = TRUE, pattern = ".rds")
list_gps <- lapply(mysample, readRDS)
track_names <- tools::file_path_sans_ext(list.files("Data/LPPI2019/GPStxt/Cleaned", pattern = ".rds"))
names(list_gps) <- track_names


for (i in 1:length(list_gps)) {
  lpproj <- list_gps[[i]]
  
  #head(lpproj)
  
  #world <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
  
  #2. Project the original data####
  # Prepare CRS Strings
  # CRS for WGS84
  proj.latlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  #CRS string for australia
  ESPG <- x<-rgdal::make_EPSG()
  myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
  proj.aeqd <- myESPG$prj4
  
  #Transform to spatial object 
  coordinates(lpproj) <- ~ Longitude + Latitude
  
  # Define CRS
  proj4string(lpproj) <- CRS(proj.latlon)
  
  # Project the data
  lpproj <- spTransform(lpproj, CRS(proj.aeqd))
  
  #Prepare for momentuHMM
  obstimes <- as.numeric(lpproj$Dt) / 3600 # convert time to numeric hours
  lnError <- crawl::argosDiag2Cov(50,50,0) # assume 50m isotropic error ellipse
  lpdata <- data.frame(ID=1,
                       time=obstimes,
                       x=lpproj@coords[, 1],
                       y=lpproj@coords[, 2],
                       ln.sd.x=lnError$ln.sd.x, 
                       ln.sd.y = lnError$ln.sd.y, 
                       error.corr= lnError$error.corr)
  
  #Check the track
  # ori_plot <- ggplot(data = world) + 
  #   geom_sf() +
  #   geom_path(data = lpdata, aes(x, y), color = "#440154FF") +
  #   geom_point(data = lpdata, aes(x, y), color = "#440154FF", alpha = 0.5) +
  #   annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1.3) +
  #   coord_sf(crs = proj.aeqd, expand = T, xlim = c(min(lpdata$x), max(lpdata$x)), ylim = c(min(lpdata$y), max(lpdata$y))) +
  #   labs(title = "original track") +
  #   theme_bw() +
  #   theme(panel.grid = element_line(color = "#C4C4C4"))
  # 
  # print(ori_plot)
  
  #3. MomentuHMM Spatial interpolation
  crwOut <- crawlWrap(lpdata,theta=c(6.5,-.1),fixPar=c(1,1,NA,NA),
                      err.model = list(x=~ln.sd.x-1,y=~ln.sd.y-1,rho=~error.corr),
                      timeStep=0.25, # predict at 15 min time steps
                      attempts=10)
  
  correc_mom <- crwOut$crwPredict
  
  # mom_plot <- ggplot(data = world) + 
  #   geom_sf() +
  #   geom_path(data = correc_mom, aes(mu.x, mu.y), color = "#FDE725FF") +
  #   geom_point(data = correc_mom, aes(mu.x, mu.y), color = "#FDE725FF", alpha = 0.5) +
  #   annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1.3) +
  #   coord_sf(crs = proj.aeqd, expand = T, xlim = c(min(lpdata$x), max(lpdata$x)), ylim = c(min(lpdata$y), max(lpdata$y))) +
  #   labs(title = "momentuHMM track") +
  #   theme_bw() +
  #   theme(panel.grid = element_line(color = "#C4C4C4"))
  # 
  # mom_plot
  # 
  # over_plot <- ggplot(data = world) +
  #   geom_sf() +
  #   geom_path(data = correc_mom, aes(mu.x, mu.y), color = "#FDE725FF") +
  #   geom_point(data = correc_mom, aes(mu.x, mu.y), color = "#FDE725FF", alpha = 0.5) +
  #   geom_path(data = lpdata, aes(x, y), color = "#440154FF") +
  #   geom_point(data = lpdata, aes(x, y), color = "#440154FF", alpha = 0.5) +
  #   annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1.3) +
  #   coord_sf(crs = proj.aeqd, expand = T, xlim = c(min(lpdata$x), max(lpdata$x)), ylim = c(min(lpdata$y), max(lpdata$y))) +
  #   labs(title = "overlaped track") +
  #   theme_bw() +
  #   theme(panel.grid = element_line(color = "#C4C4C4"))
  # 
  # over_plot
  
  correc_mom$dt <- with_tz(as_datetime(correc_mom$time * 3600), tz = "Australia/Melbourne")
  saveRDS(correc_mom, paste0("Data/LPPI2019/GPStxt/Cleaned/Interpolated/", names(list_gps)[i], ".rds"))
}
