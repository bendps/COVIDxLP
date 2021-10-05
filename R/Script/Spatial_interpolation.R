#~~~~~#
rm(list = ls())
set.seed(2498)
library(sp);library(crawl);library(tidyverse);library(rnaturalearth)
library(ggspatial);library(lubridate); library(sf);library(adehabitatLT)
#~~~~~#

#Load data and map
tbl_locs <- readRDS("Data/LPPI2020/GPStxt/Cleaned/GPS_G3002MP19.rds")
esri_ocean <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                     'Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

coordinates(tbl_locs) <- ~ Longitude + Latitude
proj4string(tbl_locs) <- "+init=epsg:4326"


mytraj <- as.ltraj(xy = tbl_locs@coords,
                   date = tbl_locs$Dt,
                   id = tbl_locs$Nest,
                   typeII = TRUE)

newtraj <- redisltraj(mytraj, 600, type = "time")

#Transform to spatial object 
sf_locs <- st_as_sf(newtraj[[1]], coords = c("x","y")) %>% 
  st_set_crs(4326)

#From spatial point to lines
sf_lines <- sf_locs %>% 
  dplyr::arrange(Nest, Dt) %>% 
  sf::st_geometry() %>% 
  sf::st_cast("MULTIPOINT",ids = as.integer(as.factor(sf_locs$Nest))) %>% 
  sf::st_cast("MULTILINESTRING") %>% 
  sf::st_sf(deployid = as.factor(unique(sf_locs$Nest)))

#Project for the australian zone (australia albers)
sf_locs <- sf::st_transform(sf_locs, 3577)
sf::st_geometry(sf_locs)


ggplot() + 
  annotation_map_tile(type = esri_ocean,zoomin = 1,progress = "none") +
  layer_spatial(sf_locs, size = 0.75) +
  scale_x_continuous(expand = expansion(mult = c(.6, .6))) +
  scale_color_brewer(palette = "Dark2") +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 1.3) +
  theme(legend.position = "none")






