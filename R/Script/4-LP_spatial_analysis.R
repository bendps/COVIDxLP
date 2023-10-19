rm(list = ls())
library(tidyverse);library(lubridate);library(sp);library(sf)
library(ggspatial); library(viridis);library(rnaturalearth)
library(adehabitatHR);library(grid); library(gtable); library(ggpubr)
library(report);library(broom); library(rempsyc)
theme_set(theme_bw())


#Load interpolated data for all seasons ####
lp_loc <- tibble()
for (myseason in 2010:2020) {
  my_global <- readRDS(paste0("Data/LPPI",myseason,"/Interpolated_GPS_",myseason,".rds"))
  
  #Keep only pred data to have 15' interval
  my_global$crwPredict <- my_global$crwPredict[which(my_global$crwPredict$locType == "p"),] 
  
  #Remove track with less than 3 data points
  too_short <- names(which(table(my_global$crwPredict$ID) < 3))
  if(length(too_short) > 0){
    my_global$crwPredict <- my_global$crwPredict[-which(my_global$crwPredict$ID %in% too_short),] 
  }
  
  rownames(my_global$crwPredict) <- 1:nrow(my_global$crwPredict)
  year_k <- as_tibble(my_global$crwPredict)
  
  year_k$season <- myseason
  
  year_k$ID <- paste0(year_k$ID,"_", year_k$season[1])
  lp_loc <- rbind(lp_loc, year_k)
}

#Add breeding stage
lp_loc$temp <- substr(lp_loc$ID,1,3)
lp_loc$stage <- str_extract(lp_loc$temp, "P")
lp_loc$stage[which(is.na(lp_loc$stage))] <- str_extract(lp_loc$temp[which(is.na(lp_loc$stage))], "G")
lp_loc$stage[which(is.na(lp_loc$stage))] <- str_extract(lp_loc$temp[which(is.na(lp_loc$stage))], "I")

#Special for 2015
lp_loc$stage[which(is.na(lp_loc$stage))] <- str_extract(lp_loc$ID[which(is.na(lp_loc$stage))], "_PG_")
lp_loc$stage[which(is.na(lp_loc$stage))] <- str_extract(lp_loc$ID[which(is.na(lp_loc$stage))], "_G_")
lp_loc$stage[which(is.na(lp_loc$stage))] <- str_extract(lp_loc$ID[which(is.na(lp_loc$stage))], "_I_")
lp_loc$stage <- gsub("_G_", "G", lp_loc$stage)
lp_loc$stage <- gsub("_PG_", "P", lp_loc$stage)
lp_loc$stage <- gsub("_I_", "I", lp_loc$stage)

lp_loc$stage_season <- paste0(lp_loc$stage, lp_loc$season)


# Create a copy of the object to make into a SpatialPointsDataFrame
# Only include three columns (id, x, and y coordinates) for estimating home ranges
lp_loc_sp <- lp_loc %>%
  dplyr::select(mu.x, mu.y, season) %>% 
  rename(lon = mu.x,
         lat = mu.y,
         id = season)

coordinates(lp_loc_sp) <- c("lon", "lat")

# CRS string for Australia
ESPG <- rgdal::make_EPSG()
myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
proj.aeqd <- myESPG$prj4
proj4string(lp_loc_sp) <- CRS(proj.aeqd)

# Re project sp object to have lon/lat
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
lp_loc_sp <- spTransform(lp_loc_sp, CRS(geo.prj)) 

kernel.ref <- kernelUD(lp_loc_sp, h = "href")

#Map UD
lp_poly_50 <- st_as_sf(getverticeshr(kernel.ref, percent = 50)) 
lp_poly_50$UD <- "50"
lp_poly_95 <- st_as_sf(getverticeshr(kernel.ref, percent = 95)) 
lp_poly_95$UD <- "95"
lp_poly <- rbind(lp_poly_50, lp_poly_95)

world_map <- ne_countries(scale = 10, returnclass = "sf")

colony <- tibble(ID = "Phillip Island",longitude = 145.1503, latitude = -38.5103)
coordinates(colony) <- ~longitude + latitude
colony <- st_as_sf(colony, coords = c("longitude, latitude"))
st_crs(colony) <- st_crs(4326) #WSG84

lp_poly$UD <- factor(lp_poly$UD, levels = c("50", "95"))

mylims <- st_bbox(st_transform(lp_poly, st_crs(28355)))
my_palette <- viridis(2)

lp_plot <- ggplot() +
  geom_sf(data = lp_poly, aes(fill = UD)) +
  geom_sf(data = world_map, fill = "antiquewhite") +
  geom_sf(data = colony, col = "black", size = 1) +
  coord_sf(crs = st_crs(28355), xlim = c(mylims[1],mylims[3]), ylim = c(mylims[2], mylims[4])) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  labs(x = "Longitude", y = "Latitude") +
  scale_fill_manual(name = "UD levels (%)",
                    values = c(
                      "95" = alpha(my_palette[2], 0.5),
                      "50" = alpha(my_palette[2], 0.9))) +
  facet_wrap(~id) +
  theme_pubr() +
  labs_pubr() +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5),
        panel.background = element_rect(fill = "#F0F8FF"),
        axis.text.x = element_text(angle = 40, hjust = 1),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 10),
        text = element_text(size = 15))

plot(lp_plot)

#Overlap analysis
library(nlme)
library(sjPlot)
library(multcomp)
library(MuMIn)

#Model 95
lp_overlap_95 <- as.data.frame.table(kerneloverlap(lp_loc_sp, method = "UDOI", percent = 95)) %>%
  filter(Var1 != Var2) %>% 
  rename(Year_A = Var1,
         Year_B = Var2)

lp_overlap_95 %>%
  group_by(Year_A) %>% 
  summarise(mean_ov = mean(Freq), n = n(), sd_ov = sd(Freq)) %>% 
  mutate(se_ov = sd_ov/sqrt(n))

ggplot(lp_overlap_95, aes(x = Year_A, y = Freq)) +
  geom_point() +
  geom_violin()

hist(lp_overlap_95$Freq)

mod_overlap_A <- glm(Freq ~ Year_A + Year_B,
                     data = lp_overlap_95,
                     family = Gamma(),
                     na.action = "na.fail")

mod_select_ov_95 <- dredge(mod_overlap_A)
mod_select_ov_95_print <- nice_table(mod_select_ov_95[,2:ncol(mod_select_ov_95)])
flextable::save_as_docx(mod_select_ov_95_print, path = "R/Figures/final_figures/mod_select_ov_95.docx")

summary(mod_overlap_A)
plot(mod_overlap_A)

plot_model(mod_overlap_A, type = "est")
plot_model(mod_overlap_A, type = "pred", ci.lvl = 0.95)
summary(glht(mod_overlap_A, linfct = multcomp::mcp(Year_A = "Tukey")), test = adjusted("holm"))

mod_tab_over_pengu <- as.data.frame(report(mod_overlap_A))
mod_tab_over_pengu <- nice_table(mod_tab_over_pengu, short = T)
flextable::save_as_docx(mod_tab_over_pengu, path = "R/Figures/final_figures/summary_over_pengu.docx")

#Model 50
lp_overlap_50 <- as.data.frame.table(kerneloverlap(lp_loc_sp, method = "UDOI", percent = 50)) %>%
  filter(Var1 != Var2) 

ggplot(lp_overlap_50, aes(x = Var1, y = Freq)) +
  geom_point() +
  geom_violin()

hist(lp_overlap_50$Freq)

mod_overlap_A_50 <- glm(Freq ~ Var1 + Var2,
                     data = lp_overlap_50,
                     family = Gamma(),
                     na.action = "na.fail")

dredge(mod_overlap_A_50)
summary(mod_overlap_A_50)
plot(mod_overlap_A_50)

plot_model(mod_overlap_A_50, type = "est")
plot_model(mod_overlap_A_50, type = "pred", ci.lvl = 0.95)

summary(glht(mod_overlap_A_50, linfct = multcomp::mcp(Var1 = "Tukey")), test = adjusted("holm"))

#clean-plot
overlap_lp_mod_data <- as_tibble(plot_model(mod_overlap_A, type = "pred", ci.lvl = 0.95)$Var1$data)
overlap_lp_mod_data$UD <- "95"
overlap_lp_mod_data_50 <- as_tibble(plot_model(mod_overlap_A_50, type = "pred", ci.lvl = 0.95)$Var1$data)
overlap_lp_mod_data_50$UD <- "50"
overlap_lp_mod_data <- rbind(overlap_lp_mod_data,overlap_lp_mod_data_50)

ggplot() +
  geom_errorbar(data = overlap_lp_mod_data, aes(x = as.factor(x), ymin = conf.low, ymax = conf.high, col = UD), width = 0.2) +
  geom_point(data = overlap_lp_mod_data, aes(x = as.factor(x), y = predicted, col = UD), shape = 21, fill = "white", size = 3) +
  scale_color_viridis(discrete = T) +
  theme_bw() +
  labs(x = "Year A",
       y = "UDOI with other years",
       col = "UD (%)")

#Overlap with fisheries ##########
#Marine traffic
marine_df <- readRDS("Data/Marine_traffic/Marine_traffic_2012-2020.rds")
marine_df$timestamp <- dmy_hms(marine_df$timestamp)
marine_df <- marine_df %>%
  filter(!is.na(timestamp)) %>% 
  filter(year(timestamp) >= 2014) %>%
  filter(month(timestamp) >= 9) %>%
  mutate(id = as.character(year(timestamp)))
marine_df$id <- paste(marine_df$id, "boat", sep = "_")
# Only include three columns (id, x, and y coordinates) for estimating home ranges
marine_df_sp <- marine_df[, c("id", "lon", "lat")]
coordinates(marine_df_sp) <- c("lon", "lat")
# Set the coordinate reference system (CRS)
proj4string(marine_df_sp) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#LP
lp_loc <- tibble()
for (myseason in 2014:2020) {
  my_global <- readRDS(paste0("Data/LPPI",myseason,"/Interpolated_GPS_",myseason,".rds"))
  
  #Keep only pred data to have 15' interval
  my_global$crwPredict <- my_global$crwPredict[which(my_global$crwPredict$locType == "p"),] 
  
  #Remove track with less than 3 data points
  too_short <- names(which(table(my_global$crwPredict$ID) < 3))
  if(length(too_short) > 0){
    my_global$crwPredict <- my_global$crwPredict[-which(my_global$crwPredict$ID %in% too_short),] 
  }
  
  rownames(my_global$crwPredict) <- 1:nrow(my_global$crwPredict)
  year_k <- as_tibble(my_global$crwPredict)
  
  year_k$season <- myseason
  
  year_k$ID <- paste0(year_k$ID,"_", year_k$season[1])
  lp_loc <- rbind(lp_loc, year_k)
}

# Only include three columns (id, x, and y coordinates) for estimating home ranges
lp_loc_sp <- lp_loc %>%
  dplyr::select(mu.x, mu.y, season) %>% 
  rename(lon = mu.x,
         lat = mu.y,
         id = season)
lp_loc_sp$id <- paste(lp_loc_sp$id, "lp", sep = "_")
coordinates(lp_loc_sp) <- c("lon", "lat")

# CRS string for Australia
ESPG <- rgdal::make_EPSG()
myESPG <- ESPG[grep("GDA94 / Australian Albers", ESPG$note),]
proj.aeqd <- myESPG$prj4
proj4string(lp_loc_sp) <- CRS(proj.aeqd)

# Re project sp object to have lon/lat
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
lp_loc_sp <- spTransform(lp_loc_sp, CRS(geo.prj)) 

marine_lp_sp <- rbind(lp_loc_sp, marine_df_sp)

#Kernel and mapping 
kernel.ref <- kernelUD(marine_lp_sp, h = "href")

#Map UD
lp_marine_poly_50 <- st_as_sf(getverticeshr(kernel.ref, percent = 50)) 
lp_marine_poly_50$UD <- "50"
lp_marine_poly_95 <- st_as_sf(getverticeshr(kernel.ref, percent = 95)) 
lp_marine_poly_95$UD <- "95"
lp_marine_poly <- rbind(lp_marine_poly_50, lp_marine_poly_95)

world_map <- ne_countries(scale = 10, returnclass = "sf")

colony <- tibble(ID = "Phillip Island",longitude = 145.1503, latitude = -38.5103)
coordinates(colony) <- ~longitude + latitude
colony <- st_as_sf(colony, coords = c("longitude, latitude"))
st_crs(colony) <- st_crs(4326) #WSG84

lp_marine_poly$UD <- factor(lp_marine_poly$UD, levels = c("50", "95"))
lp_marine_poly$year <- substr(lp_marine_poly$id, 1, 4)
lp_marine_poly$type <- substr(lp_marine_poly$id, 6, nchar(lp_marine_poly$id))
mylims <- st_bbox(st_transform(lp_marine_poly, st_crs(28355)))
my_palette <- viridis(2)

lp_marine_plot <- ggplot() +
  geom_sf(data = lp_marine_poly, aes(fill = interaction(UD,type))) +
  geom_sf(data = world_map, fill = "antiquewhite") +
  geom_sf(data = colony, col = "black", size = 5) +
  coord_sf(crs = st_crs(28355), xlim = c(mylims[1],mylims[3]), ylim = c(mylims[2], mylims[4])) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  labs(x = "Longitude", y = "Latitude") +
  scale_fill_manual(name = "UD levels (%)",
                    values = c(
                      "95.lp" = alpha(my_palette[2], 0.3),
                      "50.lp" = alpha(my_palette[2], 0.7),
                      "95.boat" = alpha(my_palette[1], 0.3),
                      "50.boat" = alpha(my_palette[1], 0.7)),
                    labels = c("Marine traffic 50% UD",
                               "Marine traffic 95% UD",
                               "Little penguins 50% UD",
                               "Little penguins 95% UD")) +
  facet_wrap(~year, nrow = 2) +
  theme_pubr() +
  labs_pubr() +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5),
        panel.background = element_rect(fill = "#F0F8FF"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        axis.text = element_text(size = 30),
        text = element_text(size = 30),
        strip.background = element_rect(fill = "white"),
        legend.direction = "horizontal")

lp_marine_plot

ggsave("R/Figures/final_figures/fig2.pdf", units = "mm", width = 180, height = 120, dpi = 600, scale = 3.5)
# shift_legend <- function(p){
#   
#   # check if p is a valid object
#   if(!"gtable" %in% class(p)){
#     if("ggplot" %in% class(p)){
#       gp <- ggplotGrob(p) # convert to grob
#     } else {
#       message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
#       return(p)
#     }
#   } else {
#     gp <- p
#   }
#   
#   # check for unfilled facet panels
#   facet.panels <- grep("^panel", gp[["layout"]][["name"]])
#   empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
#   empty.facet.panels <- facet.panels[empty.facet.panels]
#   if(length(empty.facet.panels) == 0){
#     message("There are no unfilled facet panels to shift legend into. Returning original plot.")
#     return(p)
#   }
#   
#   # establish extent of unfilled facet panels (including any axis cells in between)
#   empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
#   empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
#                              max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
#   names(empty.facet.panels) <- c("t", "l", "b", "r")
#   
#   # extract legend & copy over to location of unfilled facet panels
#   guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
#   if(length(guide.grob) == 0){
#     message("There is no legend present. Returning original plot.")
#     return(p)
#   }
#   gp <- gtable_add_grob(x = gp,
#                         grobs = gp[["grobs"]][[guide.grob]],
#                         t = empty.facet.panels[["t"]],
#                         l = empty.facet.panels[["l"]],
#                         b = empty.facet.panels[["b"]],
#                         r = empty.facet.panels[["r"]],
#                         name = "new-guide-box")
#   
#   # squash the original guide box's row / column (whichever applicable)
#   # & empty its cell
#   guide.grob <- gp[["layout"]][guide.grob, ]
#   if(guide.grob[["l"]] == guide.grob[["r"]]){
#     gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
#   }
#   if(guide.grob[["t"]] == guide.grob[["b"]]){
#     gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
#   }
#   gp <- gtable_remove_grobs(gp, "guide-box")
#   
#   return(gp)
# }
# 
# png("R/Figures/final_figures/fig2.png", units = "mm", width = 180, height = 9, units = "in", res = 400)
# grid.draw(shift_legend(lp_marine_plot))
# dev.off()

#Model overlap
#Overlap analysis
library(nlme)
library(sjPlot)
library(multcomp)

#Model 95 
marine_lp_overlap_95 <- as.data.frame.table(kerneloverlap(marine_lp_sp, method = "UDOI", percent = 95))
marine_lp_overlap_95 <- marine_lp_overlap_95 %>%
  mutate(penguin_year = substr(Var1, 1, 4),
         boat_year = substr(Var2, 1, 4),
         type_A = substr(Var1, 6, nchar(as.character(Var1))),
         type_B = substr(Var2, 6, nchar(as.character(Var2)))) %>% 
  filter(type_A != type_B,
         type_A == "lp")
  
marine_lp_overlap_95$penguin_year <- as.factor(marine_lp_overlap_95$penguin_year)
marine_lp_overlap_95$boat_year <- as.factor(marine_lp_overlap_95$boat_year)

marine_lp_overlap_95 %>%
  group_by(penguin_year) %>% 
  summarise(mean = mean(Freq),
            se = sd(Freq)/sqrt(n()))

ggplot(marine_lp_overlap_95, aes(x = penguin_year, y = Freq)) +
  geom_point() +
  geom_violin()

mod_overlap_A <- glm(Freq ~ penguin_year + boat_year,
                     data = marine_lp_overlap_95,
                     family = Gamma(),
                     na.action = "na.fail")

mod_select_pengu_vessel <- dredge(mod_overlap_A)
mod_select_pengu_vessel_print <- nice_table(mod_select_pengu_vessel[,2:ncol(mod_select_pengu_vessel)])
flextable::save_as_docx(mod_select_pengu_vessel_print, path = "R/Figures/final_figures/mod_select_pengu_vessel.docx")

mod_overlap_A_final <- glm(Freq ~ penguin_year,
                     data = marine_lp_overlap_95,
                     family = Gamma(),
                     na.action = "na.fail")

summary(mod_overlap_A_final)
plot(mod_overlap_A_final)
mod_tab_pengu_vessel <- as.data.frame(report(mod_overlap_A_final))
mod_tab_pengu_vessel <- nice_table(mod_tab_pengu_vessel, short = T)
flextable::save_as_docx(mod_tab_pengu_vessel, path = "R/Figures/final_figures/summary_pengu_vessel.docx")

as_tibble(plot_model(mod_overlap_A_final, type = "pred", ci.lvl = 0.95)$penguin_year$data)
summary(glht(mod_overlap_A_final, linfct = multcomp::mcp(penguin_year = "Tukey")), test = adjusted("holm"))

#clean-plot
overlap_marine_lp_mod_data <- as_tibble(plot_model(mod_overlap_B, type = "pred", ci.lvl = 0.95)$year_A$data)

ggplot() +
  geom_errorbar(data = overlap_marine_lp_mod_data, aes(x = as.factor(x), ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_point(data = overlap_marine_lp_mod_data, aes(x = as.factor(x), y = predicted), shape = 21, fill = "white", size = 3) +
  theme_bw() +
  labs(x = "Year A",
       y = "UDOI with other years",
       col = "UD (%)")


#Marine traffic type histogram for supp
marine_df$simple_type <- str_to_title(sub("([A-Za-z]+).*", "\\1", marine_df$type))
table(marine_df$simple_type)
marine_df$simple_type <- ifelse(marine_df$simple_type %in% c(names(which(table(marine_df$simple_type) > 1000))),
                                marine_df$simple_type,
                                "Other")

grp_marine_df <- marine_df %>% 
  mutate(year = year(timestamp)) %>% 
  group_by(simple_type, year) %>%
  summarise(n = n())

ggplot(grp_marine_df, aes(x = year, y = n, fill = simple_type)) +
  theme_pubr() +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_viridis(discrete = T, option = "turbo") +
  labs(x ="Year", y = "Vessel type proportion", fill = "Vessel type") +
  labs_pubr() + 
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        text = element_text(size = 20))
ggsave("R/Figures/final_figures/supfig1.pdf", units = "mm", width = 80, height = 60, dpi = 600, scale = 3.5)

ggplot(marine_df, aes(x = year(timestamp), fill = simple_type)) +
  theme_pubr() +
  geom_bar(position = "stack") +
  scale_fill_viridis(discrete = T, option = "turbo") +
  labs(x ="Year", y = "Number of vessels", fill = "Vessel type") +
  scale_y_continuous(breaks = c(seq(0,30000,10000)),
                     labels = c("0", "10 k", "20 k", "30 k")) + 
  labs_pubr() + 
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        text = element_text(size = 20))

ggsave("R/Figures/final_figures/supfig1_number.pdf", units = "mm", width = 80, height = 60, dpi = 600, scale = 3.5)
