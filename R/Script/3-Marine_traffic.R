#~~~~~#
rm(list = ls())
library(viridis);library(proj4);library(tidyverse);library(ggspatial)
library(lubridate);library(foreign);library(rnaturalearth);library(raster)
library(track2KBA); library(sf); library(ggnewscale)
#~~~~~#

#Prepare dataset ###########################################################################################################################
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

#Compute kernels ############################################################################################################################
marine_df <- readRDS("Data/Marine_traffic/Marine_traffic_2012-2020.rds")
marine_df$timestamp <- dmy_hms(marine_df$timestamp)
marine_df <- marine_df %>%
  filter(!is.na(timestamp)) %>% 
  filter(year(timestamp) >= 2014) %>%
  filter(month(timestamp) >= 9) %>%
  mutate(id = as.character(year(timestamp)))

# Create a copy of the object to make into a SpatialPointsDataFrame
# Only include three columns (id, x, and y coordinates) for estimating home ranges
marine_df_sp <- marine_df[, c("id", "lon", "lat")]
coordinates(marine_df_sp) <- c("lon", "lat")

# Set the coordinate reference system (CRS)
proj4string(marine_df_sp) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

kernel.ref <- kernelUD(marine_df_sp, h = "href")


#Map UD
marine_poly_50 <- st_as_sf(getverticeshr(kernel.ref, percent = 50)) 
marine_poly_50$UD <- "50"
marine_poly_95 <- st_as_sf(getverticeshr(kernel.ref, percent = 95)) 
marine_poly_95$UD <- "95"
marine_poly <- rbind(marine_poly_50, marine_poly_95)

marine_poly$year <- substr(marine_poly$id, 1, 4)
marine_poly$month <- sub(".*-", "", marine_poly$id)

world_map <- ne_countries(scale = 10, returnclass = "sf")

colony <- tibble(ID = "Phillip Island",longitude = 145.1503, latitude = -38.5103)
coordinates(colony) <- ~longitude + latitude
colony <- st_as_sf(colony, coords = c("longitude, latitude"))
st_crs(colony) <- st_crs(4326) #WSG84

marine_poly$UD <- factor(marine_poly$UD, levels = c("50", "95"))

mylims <- st_bbox(st_transform(marine_poly, st_crs(28355)))
my_palette <- viridis(1)

marine_plot <- ggplot() +
  geom_sf(data = marine_poly, aes(fill = UD)) +
  geom_sf(data = world_map, fill = "antiquewhite") +
  geom_sf(data = colony, col = "black", size = 1) +
  coord_sf(crs = st_crs(28355), xlim = c(mylims[1],mylims[3]), ylim = c(mylims[2], mylims[4])) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  labs(x = "Longitude", y = "Latitude") +
  scale_fill_manual(name = "UD levels (%)",
                    values = c(
                      "95" = alpha(my_palette[1], 0.5),
                      "50" = alpha(my_palette[1], 0.9))) +
  facet_grid(year~month) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5),
        panel.background = element_rect(fill = "#F0F8FF"),
        axis.text.x = element_text(angle = 40, hjust = 1))

plot(marine_plot)

#Overlap analysis
marine_overlap_95 <- as.data.frame.table(kerneloverlap(marine_df_sp, method = "UDOI", percent = 95))
marine_overlap_95 <- marine_overlap_95 %>%
  mutate(year_A = substr(Var1, 1, 4),
         year_B = substr(Var2, 1, 4),) %>% 
  filter(year_A != year_B)

marine_overlap_95$year_A <- as.factor(marine_overlap_95$year_A)
marine_overlap_95$year_B <- as.factor(marine_overlap_95$year_B)

marine_overlap_95 %>%
  group_by(Var1) %>% 
  summarise(mean = mean(Freq),
            se = sd(Freq)/sqrt(n()))

#Model
library(nlme)
library(sjPlot)
library(multcomp)
ggplot(marine_overlap_95, aes(x = year_A, y = Freq)) +
  geom_point() +
  geom_violin()

hist(marine_overlap_95$Freq)

mod_overlap_A <- glm(Freq ~ year_A + year_B,
    data = marine_overlap_95,
    family = Gamma())

mod_overlap_B <- glm(Freq ~ year_B,
                   data = marine_overlap_95,
                   family = Gamma())

mod_overlap_C <- glm(Freq ~ year_A,
                   data = marine_overlap_95,
                   family = Gamma())

mod_overlap_null <- glm(Freq ~ 1,
                   data = marine_overlap_95,
                   family = Gamma())

AIC(mod_overlap_A, mod_overlap_B, mod_overlap_C, mod_overlap_null)
summary(mod_overlap_A)
plot(mod_overlap_A)

plot_model(mod_overlap_A, type = "est")
plot_model(mod_overlap_A, type = "pred", ci.lvl = 0.95)

summary(glht(mod_overlap_A, linfct = multcomp::mcp(year_A = "Tukey")), test = adjusted("holm"))

#clean-plot
overlap_marine_mod_data <- as_tibble(plot_model(mod_overlap_A, type = "pred", ci.lvl = 0.95)$year_A$data)
ggplot() +
  geom_errorbar(data = overlap_marine_mod_data, aes(x = as.factor(x), ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_point(data = overlap_marine_mod_data, aes(x = as.factor(x), y = predicted), shape = 21, fill = "white", size = 3) +
  geom_jitter(data = marine_overlap_95, aes(x = year_A, y = Freq), width = 0.2, alpha = 0.5) +
  theme_bw() +
  labs(x = "Year A",
       y = "UDOI with year B")


