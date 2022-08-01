#Open all the packages needed
library(tidyverse)
library(sf)
library(knitr)
library(terra)
library(raster)
library(prioritizr)
library(units)
library(patchwork)
library(mapview)
library(viridis)
library(ggthemes)
library(rnaturalearth)
library(rgdal)
library(tmap)
library(openxlsx)
library(xlsx)

#Open rds
result_BioServ <- readRDS("RDS/result_BioServ.rds")
result_BioServ_WDPA <- readRDS("RDS/result_BioServ_WDPA.rds")
result_Bio <- readRDS("RDS/result_Bio.rds")
result_Bio_WDPA <- readRDS("RDS/result_Bio_WDPA.rds")
PUs <- readRDS("RDS/PUs_Splitted.rds")
PUs_NotSplitted <- readRDS("RDS/PUs_NotSplitted.rds")
ntarget_reached_df_BioServ <- readRDS("RDS/ntarget_reached_df_BioServ.rds")
ntarget_reached_df_BioServ_WDPA <- readRDS("RDS/ntarget_reached_df_BioServ_WDPA.rds")

#Ecosystem services histogram

fish_hist <- ggplot(data = PUs, aes(x = log(Fishing_Intensity + 1))) +
  geom_histogram(colour = "black", binwidth = 0.5, fill = "#1363DF") +
  theme_bw(base_size = 8) +
  xlab(expression(Fishing~Intensity~log(fisher~days~km^{-2}~year^{-1})))

prop_hist <- ggplot(data = PUs, aes(x = log(TOT_STOCK + 1))) +
  geom_histogram(colour = "black", binwidth = 0.5, fill = "#EB5C90") +
  theme_bw(base_size = 8) +
  xlab(expression(Properties~protected~log("$"~km^{-2})))
  
pop_hist <- ggplot(data = PUs, aes(x = log(POP + 1))) +
  geom_histogram(colour = "black", binwidth = 0.5, fill = "#CC9900") +
  theme_bw(base_size = 8) +
  xlab(expression(Population~protected~log(people~km^{-2})))

carb_hist <- ggplot(data = PUs, aes(x = Tot_Carbon)) +
  geom_histogram(colour = "black", binwidth = 0.5, fill = "#00B899") +
  theme_bw(base_size = 8) +
  xlab(expression(Carbon~stored~(Mg~km^{-2})))

ggsave(plot = fish_hist, "Figures/Hist_Fish.pdf", 
       dpi = 1000, width = 8, height = 8, units = "cm", limitsize = FALSE)

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
  filter(Group %in% c(10, 30, 50))

plot_radar_BioServ <- fPlot_Radar(radar_data)

ggsave(plot = plot_radar_BioServ, "Figures/Radar.svg", 
       dpi = 1000, width = 4, height = 4, units = "cm", limitsize = FALSE)

### Building on WDPA

ntarget_reached_df_BioServ_WDPA <- ntarget_reached_df_BioServ_WDPA %>% 
  arrange(prct)

Increase_EcoServices_WDPA_Prct <- Increase_EcoServices_WDPA_Prct %>%
  filter(prct != 27.99) %>% 
  arrange(prct)

radar_data_WDPA <- Increase_EcoServices_WDPA_Prct %>% 
  mutate(Biodiversity = ntarget_reached_df_BioServ_WDPA$reached/100) %>% 
  rename(Group = prct) %>% 
  dplyr::select(Group, everything()) %>% 
  filter(Group %in% c(28, 30, 50))

plot_radar_BioServ_WDPA <- fPlot_Radar(radar_data_WDPA)

ggsave(plot = plot_radar_BioServ_WDPA, "Figures/Radar_WDPA.svg", dpi = 1000, width = 4, height = 4, units = "cm", limitsize = FALSE)

# People and properties inside and outside already protected areas
PUs %>% 
  as_tibble() %>% 
  group_by(Protected) %>% 
  summarise(People = sum(POP*AreaGMWKm),
            Properties = sum(TOT_STOCK*AreaGMWKm))

