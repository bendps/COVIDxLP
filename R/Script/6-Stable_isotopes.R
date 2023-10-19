rm(list = ls())
set.seed(2498)
library(tidyverse);library(cowplot);library(lubridate); library(nicheROVER)
library(viridis);library(SIBER);library(ellipse); library(coda); library(ggpubr)
library(sjPlot)
str(pdfFonts("sans"))
isotope <- read.csv("Data/lp_isotope.csv", sep = ";")

# Format dataset
names(isotope) <- tolower(names(isotope))
isotope <- isotope %>% filter(year >= 2010)
isotope <- isotope %>% filter(b.stage != "Pre-breeding")

#1. SIBER ####
overlap_tibble <- tibble(season_a = NA, season_b = NA, stage = NA, overlap = NA)
ellipse.plot <- list()
area_ggplot <- list()
for (mystage in unique(isotope$b.stage)) {
  my_iso <- isotope %>% filter(b.stage == mystage)
  
  # Format for SIBER
  my_iso <- as.data.frame(cbind(my_iso$delta.c, my_iso$delta.n, my_iso$year,my_iso$b.stage))
  names(my_iso) <- c("iso1", "iso2", "group", "community")
  my_iso$community <- as.numeric(as.factor(my_iso$community))
  my_iso$iso1 <- as.numeric(my_iso$iso1)
  my_iso$iso2 <- as.numeric(my_iso$iso2)
  
  siber.lp <- createSiberObject(my_iso)
  
  # Plot isotopic niches 
  #GGplot version
  p.ell <- 0.4
  
  ellipse.plot[[mystage]] <- ggplot(data = my_iso, aes(x = iso1, y = iso2)) + 
    geom_point(aes(color = group), size = 3) +
    ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
    xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
    stat_ellipse(aes(group = group, 
                     fill = group, 
                     color = group), 
                 alpha = 0.1, 
                 level = p.ell,
                 type = "norm",
                 geom = "polygon") + 
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_pubr() +
    labs_pubr() +
    labs(fill = "Year",
         color = "Year") +
    theme(legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          axis.text = element_text(size = 20),
          text = element_text(size = 20),
          legend.position = "none")
  
  print(ellipse.plot[[mystage]])
 

  # Calculate summary statistics for each group: TA, SEA and SEAc
  group.ML <- groupMetricsML(siber.lp)
  print(group.ML)
  
  
  #for (season_a in as.numeric(unique(my_iso$group))) {
  for (season_b in as.numeric(unique(my_iso$group))) {
    # So in this example: community 1, group 2
    ellipse1 <- paste0("1.", season_b)
    
    # Ellipse two is similarly defined: community 1, group3
    ellipse2 <- "1.2020"#paste0("1.", season_a)
    
    # the overlap betweeen the corresponding 95% prediction ellipses is given by:
    ellipse.overlap <- maxLikOverlap(ellipse1, ellipse2, siber.lp, 
                                     p.interval = 0.40, n = 100, do.plot = T)
    my_overlap <- ellipse.overlap[3]/ellipse.overlap[1]
    temp_tibble <- tibble(season_a = 2020, season_b = season_b, stage = mystage, overlap = my_overlap)
    overlap_tibble <- rbind(overlap_tibble,temp_tibble)
  }
  #}
  
  # overlap_tibble_filter <- overlap_tibble %>% na.omit() %>% filter(season_a != season_b)
  # ggplot(overlap_tibble_filter, aes(x = as.factor(season_a), y = overlap)) + geom_point() + facet_wrap(~season_b)
  
  # Options for running jags
  parms <- list()
  parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
  parms$n.burnin <- 1 * 10^3 # discard the first set of values
  parms$n.thin <- 10     # thin the posterior by this many
  parms$n.chains <- 2        # run this many chains
  parms$save.output <- TRUE # set save.output = TRUE
  parms$save.dir <- tempdir() #paste0(getwd(),"/Data/Stable_isotopes_models") #might want to change the directory to your local directory 
  
  # Define the priors
  priors <- list()
  priors$R <- 1 * diag(2)
  priors$k <- 2
  priors$tau.mu <- 1.0E-3
  
  # fit the ellipses which uses an Inverse Wishart prior
  # on the covariance matrix Sigma, and a vague normal prior on the 
  # means. Fitting is via the JAGS method.
  ellipses.posterior <- siberMVN(siber.lp, parms, priors)
  
  model.files <- list.files(parms$save.dir, "jags_output", full.names = TRUE) #find model temp files

  SEA.B <- siberEllipses(ellipses.posterior)

  # Calculate some credible intervals
  cr.p <- c(0.95, 0.99) # vector of quantiles
  
  # call to hdrcde:hdr using lapply()
  SEA.B.credibles <- lapply(
    as.data.frame(SEA.B),
    function(x,...){tmp<-hdrcde::hdr(x)$hdr},
    prob = cr.p)
  
  # do similar to get the modes, taking care to pick up multimodal posterior
  # distributions if present
  SEA.B.modes <- lapply(
    as.data.frame(SEA.B),
    function(x,...){tmp<-hdrcde::hdr(x)$mode},
    prob = cr.p, all.modes=T)
  
  
  #Df SEAB
  my_ci <- as_tibble(t(bind_rows(SEA.B.credibles)))
  colnames(my_ci) <- c("%99","%95","%50")
  my_ci$year <- rep(as.numeric(unique(my_iso$group)), each = 2)
  my_ci$conf_type <- rep(c("low", "high"), length(as.numeric(unique(my_iso$group))))
  
  my_mode <- as.tibble(t(bind_rows(SEA.B.modes)))
  colnames(my_mode) <- "mode"
  my_mode$year <- rep(as.numeric(unique(my_iso$group)), 1)
  
  area_df <- tibble(year = as.numeric(unique(my_iso$group)),
                    mode = NA,
                    conf_99_low = NA, conf_99_high = NA,
                    conf_95_low = NA, conf_95_high = NA,
                    conf_50_low = NA, conf_50_high = NA, 
                    SEAc = group.ML[3,])
  
  for (ii in 1:nrow(area_df)) {
    area_df$mode[ii] <- my_mode$mode[which(my_mode$year == area_df$year[ii])]
    
    area_df$conf_99_low[ii] <- my_ci$`%99`[which(my_ci$conf_type == "low" & my_ci$year == area_df$year[ii])]
    area_df$conf_99_high[ii] <- my_ci$`%99`[which(my_ci$conf_type == "high" & my_ci$year == area_df$year[ii])]
    
    area_df$conf_95_low[ii] <- my_ci$`%95`[which(my_ci$conf_type == "low" & my_ci$year == area_df$year[ii])]
    area_df$conf_95_high[ii] <- my_ci$`%95`[which(my_ci$conf_type == "high" & my_ci$year == area_df$year[ii])]
    
    area_df$conf_50_low[ii] <- my_ci$`%50`[which(my_ci$conf_type == "low" & my_ci$year == area_df$year[ii])]
    area_df$conf_50_high[ii] <- my_ci$`%50`[which(my_ci$conf_type == "high" & my_ci$year == area_df$year[ii])]
  }
  
  area_ggplot[[mystage]] <- ggplot(area_df, aes(x = as.factor(year))) +
    geom_linerange(aes(ymin = conf_99_low, ymax = conf_99_high, col = "99%")) +
    geom_linerange(aes(ymin = conf_95_low, ymax = conf_95_high, col = "95%"), size = 2) +
    geom_linerange(aes(ymin = conf_50_low, ymax = conf_50_high, col = "50%"), size = 4) +
    scale_color_manual(values = rev(viridis(3))) +
    geom_point(aes(y = mode), shape = 19, size = 2) +
    geom_point(aes(y = SEAc), shape = 1, size = 2) +
    theme_bw() +
    theme_pubr() +
    labs_pubr() +
    geom_hline(yintercept = area_df$conf_95_low[which(area_df$year == 2020)], linetype = "dashed") +
    geom_hline(yintercept = area_df$conf_95_high[which(area_df$year == 2020)], linetype = "dashed") +
    labs(x = "Years", y = expression("Standard Ellipse Area " ('\u2030' ^2)), col = "CI level") +
    theme(legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          axis.text = element_text(size = 20),
          text = element_text(size = 20),
          legend.position = "none")

}

original_area <- area_ggplot
original_ellipse <- ellipse.plot
#final plot area
dummy_area_legend <- ggplot(area_df, aes(x = as.factor(year))) +
  geom_linerange(aes(ymin = conf_99_low, ymax = conf_99_high, col = "99%")) +
  geom_linerange(aes(ymin = conf_95_low, ymax = conf_95_high, col = "95%"), size = 2) +
  geom_linerange(aes(ymin = conf_50_low, ymax = conf_50_high, col = "50%"), size = 4) +
  scale_color_manual(values = rev(viridis(3))) +
  geom_point(aes(y = mode), shape = 19, size = 2) +
  geom_point(aes(y = SEAc), shape = 1, size = 2) +
  theme_bw() +
  theme_pubr() +
  labs_pubr() +
  geom_hline(yintercept = area_df$conf_95_low[which(area_df$year == 2020)], linetype = "dashed") +
  geom_hline(yintercept = area_df$conf_95_high[which(area_df$year == 2020)], linetype = "dashed") +
  labs(x = "Years", y = expression("Standard Ellipse Area " ('\u2030' ^2)), col = "CI level") +
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        text = element_text(size = 20))

area_ggplot$Incubation <- area_ggplot$Incubation + labs(title = "A.", y = "", x = "")
area_ggplot$Guard <- area_ggplot$Guard + labs(title = "B.", x = "", y = "Standard ellipse area")
area_ggplot$`Post-guard` <- area_ggplot$`Post-guard` + labs(title = "C.", y = "")
cowplot::plot_grid(get_legend(dummy_area_legend),cowplot::plot_grid(plotlist = area_ggplot, ncol = 1), ncol = 1, rel_heights = c(0.05,0.95))
ggsave(filename = paste0("R/Figures/final_figures/fig5.pdf"), units = "mm", width = 80, height = 100, dpi = 600, scale = 3.5)
area_ggplot <- original_area 


#final plot ellispes
dummy_ell_legend <- ggplot(data = my_iso, aes(x = iso1, y = iso2)) + 
  geom_point(aes(color = group), size = 3) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  stat_ellipse(aes(group = group, 
                   fill = group, 
                   color = group), 
               alpha = 0.1, 
               level = p.ell,
               type = "norm",
               geom = "polygon") + 
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_pubr() +
  labs_pubr() +
  labs(fill = "Year",
       color = "Year") +
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        text = element_text(size = 20))

ellipse.plot$Incubation <- ellipse.plot$Incubation + labs(title = "A.", y = "", x = "") + coord_cartesian(xlim = c(-23.5, -18.5), ylim = c(12.5, 20.5))
ellipse.plot$Guard <- ellipse.plot$Guard + labs(title = "B.", x = "") + coord_cartesian(xlim = c(-23.5, -18.5), ylim = c(12.5, 20.5))
ellipse.plot$`Post-guard` <- ellipse.plot$`Post-guard` + labs(title = "C.", y = "") + coord_cartesian(xlim = c(-23.5, -18.5), ylim = c(12.5, 20.5))
cowplot::plot_grid(get_legend(dummy_ell_legend),cowplot::plot_grid(plotlist = ellipse.plot, ncol = 1), ncol = 1, rel_heights = c(0.05,0.95))
ggsave(filename = paste0("R/Figures/final_figures/fig4.png"), units = "mm", width = 80, height = 100, dpi = 600, scale = 3.5)
ellipse.plot <- original_ellipse
