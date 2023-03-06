#Author: Alvise Dabalà

#Code to prepare the barplot and the radar plot presented in the study

#Open all the packages needed
library(tidyverse)
library(sf)
library(prioritizr)
library(patchwork)
library(viridis)
library(ggthemes)

#Open rds
result_BioServ <- readRDS("RDS_rr/1e-4/result_BioServ.rds")
result_BioServ_WDPA <- readRDS("RDS_rr/1e-4/result_BioServ_WDPA.rds")
result_Bio <- readRDS("RDS_rr/1e-4/result_Bio.rds")
result_Bio_WDPA <- readRDS("RDS_rr/result_Bio_WDPA.rds")
result_Bio_AllWDPA <- readRDS("RDS_rr/1e-4/result_Bio_AllWDPA.rds")
result_BioServ_AllWDPA <- readRDS("RDS_rr/1e-4/result_BioServ_AllWDPA.rds")
PUs <- readRDS("RDS_rr/PUs_Splitted_I_IV_and_All_9111.rds")
PUs_NotSplitted <- readRDS("RDS/PUs_NotSplitted.rds")
ntarget_reached_df_BioServ <- readRDS("RDS_rr/ntarget_reached_df_BioServ.rds")
ntarget_reached_df_BioServ_WDPA <- readRDS("RDS_rr/ntarget_reached_df_BioServ_WDPA.rds")
ntarget_reached_df_BioServ_AllWDPA <- readRDS("RDS_rr/ntarget_reached_df_BioServ_AllWDPA.rds")
Increase_EcoServices_Prct <- readRDS("RDS_rr/Increase_EcoServices_Prct.rds")
Increase_EcoServices_WDPA_Prct <- readRDS("RDS_rr/Increase_EcoServices_WDPA_Prct.rds")
Increase_EcoServices_AllWDPA_Prct <- readRDS("RDS_rr/Increase_EcoServices_AllWDPA_Prct.rds")

#Upload functions
source("Functions/fPlot_Radar.r")

#Ecosystem services histogram

fish_hist <- ggplot(data = PUs, aes(x = log10(Fishing_Intensity + 1))) +
  geom_histogram(colour = "black", binwidth = 0.5, fill = "#1363DF") +
  theme_bw(base_size = 8) +
  xlab(expression(Fishing~Intensity~Log[10](fisher~days~km^{-2}~year^{-1}))) +
  ylab("Number of planning units")

prop_hist <- ggplot(data = PUs, aes(x = log(TOT_STOCK + 1))) +
  geom_histogram(colour = "black", binwidth = 0.5, fill = "#EB5C90") +
  theme_bw(base_size = 8) +
  xlab(expression(Properties~protected~Log[10]("$"~km^{-2}))) +
  ylab("Number of planning units")
  
pop_hist <- ggplot(data = PUs, aes(x = log10(POP + 1))) +
  geom_histogram(colour = "black", binwidth = 0.5, fill = "#CC9900") +
  theme_bw(base_size = 8) +
  xlab(expression(Population~protected~Log[10](people~km^{-2}))) +
  ylab("Number of planning units")

carb_hist <- ggplot(data = PUs, aes(x = Tot_Carbon)) +
  geom_histogram(colour = "black", binwidth = max(PUs$Tot_Carbon)/20, fill = "#00B899") +
  theme_bw(base_size = 8) +
  xlab(expression(Carbon~stored~(Mt~km^{-2}))) +
  ylab("Number of planning units")

prop_hist + pop_hist +
  carb_hist + fish_hist +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a') +
  theme(plot.tag = element_text(face = 'bold'))

ggsave("Figures/Hist.pdf", 
       dpi = 1000, width = 16, height = 16, units = "cm", limitsize = FALSE)

ggsave(plot = prop_hist, "Figures/Hist_Properties.pdf", 
       dpi = 1000, width = 8, height = 8, units = "cm", limitsize = FALSE)

ggsave(plot = pop_hist, "Figures/Hist_Population.pdf", 
       dpi = 1000, width = 8, height = 8, units = "cm", limitsize = FALSE)

ggsave(plot = carb_hist, "Figures/Hist_Carbon.pdf", 
       dpi = 1000, width = 8, height = 8, units = "cm", limitsize = FALSE)

################################################################################
# Radar plot
ntarget_reached_df_BioServ <- ntarget_reached_df_BioServ %>% 
  arrange(prct)

Increase_EcoServices_Prct <- Increase_EcoServices_Prct %>%
  add_row(tibble(Fishing = 0, People = 0, Properties = 0, Carbon = 0, prct = 0)) %>% 
  arrange(prct)
  
radar_data <- Increase_EcoServices_Prct %>% 
  mutate(Biodiversity = ntarget_reached_df_BioServ$reached/100) %>% 
  rename(Group = prct) %>% 
  dplyr::select(Group, everything()) %>% 
  filter(Group %in% c(10, 30))

plot_radar_BioServ <- fPlot_Radar(radar_data)

ggsave(plot = plot_radar_BioServ, "Figures_rr/Radar.svg", 
       dpi = 1000, width = 4, height = 4, units = "cm", limitsize = FALSE)

### Building on WDPA

ntarget_reached_df_BioServ_WDPA <- ntarget_reached_df_BioServ_WDPA %>% 
  arrange(prct) 

Increase_EcoServices_WDPA_Prct <- Increase_EcoServices_WDPA_Prct %>% 
  arrange(prct) 

Increase_EcoServices_WDPA_Prct <- Increase_EcoServices_WDPA_Prct %>% 
  left_join(ntarget_reached_df_BioServ_WDPA, by = "prct") %>% 
  dplyr::select(!method) %>% 
  replace(is.na(.), 0) %>% 
  rename(Biodiversity = reached)

radar_data_WDPA <- Increase_EcoServices_WDPA_Prct %>%
  mutate(Biodiversity = Biodiversity/100) %>% 
  rename(Group = prct) %>% 
  dplyr::select(Group, everything()) %>% 
  filter(Group %in% c(13.5, 30))

plot_radar_BioServ_WDPA <- fPlot_Radar(radar_data_WDPA)

ggsave(plot = plot_radar_BioServ_WDPA, "Figures_rr/Radar_WDPA.svg", dpi = 1000, width = 4, height = 4, units = "cm", limitsize = FALSE)

### Building on AllDPA

ntarget_reached_df_BioServ_AllWDPA <- ntarget_reached_df_BioServ_AllWDPA %>% 
  arrange(prct) 

Increase_EcoServices_AllWDPA_Prct <- Increase_EcoServices_AllWDPA_Prct %>% 
  arrange(prct) 

Increase_EcoServices_AllWDPA_Prct <- Increase_EcoServices_AllWDPA_Prct %>% 
  left_join(ntarget_reached_df_BioServ_WDPA, by = "prct") %>% 
  dplyr::select(!method) %>% 
  replace(is.na(.), 0) %>% 
  rename(Biodiversity = reached)

radar_data_AllWDPA <- Increase_EcoServices_AllWDPA_Prct %>%
  mutate(Biodiversity = Biodiversity/100) %>% 
  rename(Group = prct) %>% 
  dplyr::select(Group, everything()) %>% 
  filter(Group %in% c(43, 50))

plot_radar_BioServ_AllWDPA <- fPlot_Radar(radar_data_AllWDPA)

ggsave(plot = plot_radar_BioServ_AllWDPA, "Figures_rr/Radar_AllWDPA.svg", dpi = 1000, width = 4, height = 4, units = "cm", limitsize = FALSE)
