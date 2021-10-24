#~~~~~#
rm(list = ls())
set.seed(2498)
library(viridis);library(proj4);library(tidyverse);library(ggspatial)
library(lubridate);library(foreign);library(rnaturalearth);library(raster)
#~~~~~#

myseason <- 2019

marine_path <- paste0("Data/Marine_traffic/Marine_traffic_", myseason,"/dbf_files")
marine_files <- list.files(marine_path, pattern = "*.dbf")

marine_df <- tibble()
for (i in 1:length(marine_files)) {
  my_df <- read.dbf(paste0(marine_path, "/", marine_files[i]))
  marine_df <- rbind(marine_df, my_df)
  print(paste0(i,"/", length(marine_files)))
}

names(marine_df) <- tolower(names(marine_df))
marine_df_filtered <- marine_df %>% filter(lat <= -38.5 & lat >= -39.5 & lon >= 145 & lon <= 146)
head(marine_df_filtered)

saveRDS(marine_df_filtered, paste0("Data/Marine_traffic/Marine_traffic_", myseason,".rds"))


#Now concatenate every season
marine_path <- paste0("Data/Marine_traffic")
marine_files <- list.files(marine_path, pattern = "*.rds")

marine_df <- tibble()
for (i in 3:length(marine_files)) {
  my_df <- readRDS(paste0(marine_path,"/", marine_files[i]))
  marine_df <- rbind(marine_df, my_df)
  print(paste0(i,"/", length(marine_files)))
}

saveRDS(marine_df, "Data/Marine_traffic/Marine_traffic_2012-2020.rds")




coordinates(marine_df_filtered) <- ~ lon + lat

#rasterize
r <- raster(xmn=145, ymn=-39.5, xmx=146, ymx=-38.5, res=0.025)
r.nb <- rasterize(marine_df_filtered, r, field = marine_df_filtered$craft_id , fun='count')  # field can be any column with fun="count"

# plot ggplot, il faut repasser en data.frame
df.nb <- as.data.frame(r.nb,xy=T)
colnames(df.nb)[which(names(df.nb) == "layer")] <- "myvariable"
df.nb$myvariable[which(is.na(df.nb$myvariable))] <- 0

world <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")

mygps <- readRDS("Data/LPPI2020/Interpolated_GPS_Depth_2020.rds")
mygps <- mygps$crwPredict

coordinates(mygps) <- mu.x + mu.y

ggplot(data = world) +
  geom_raster(data = df.nb, aes( x = x, y = y, fill = myvariable)) +
  scale_fill_gradientn(colors = c("#FFFFFF00","#E1E1E1",rev(viridis(30)))) +
  geom_sf() +
  geom_point(data = mygps, aes(mu.x, mu.y)) +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1.3) +
  coord_sf(xlim = c(145,146), ylim = c(-39.5,-38.4), expand = F)
  
