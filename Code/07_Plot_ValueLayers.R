#Author: Alvise Dabalà

#Code to produce the plots of the data in input

#Open all the packages needed
pacman::p_load(tidyverse, sf, prioritizr, patchwork, viridis, ggthemes, rnaturalearth)

#Here I produce the graphs that report the values of the ecosystem benefits and
#biodiversity benefits for each planning unit

cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

source("Functions/fPlot_PUsValues.R")

species <- readRDS("RDS/species.rds")
PUs <- readRDS("RDS/PUs_Splitted_I_IV_and_All_9111.rds")
PUs_NotSplitted <- readRDS("RDS/PUs_NotSplitted.rds")
Large_PUs <- readRDS("RDS/Large_PUs.rds")
species <- readRDS("RDS/species.rds")

#Plot of fishing intensity
plot_fish <- fPlot_PUsValues(PUs, "Fishing_Intensity", logarithmic = TRUE, scale_fill = "mako")
ggsave(filename = "Figures/Fishing_Intensity_log.svg", plot = plot_fish, width = 49, height = 25)

#Plot of properites protected
plot_properties <- fPlot_PUsValues(PUs, "TOT_STOCK", logarithmic = TRUE, scale_fill = "cividis")
ggsave(filename = "Figures/Properties_log.svg", plot = plot_properties, width = 49, height = 25)

#Plot of population protected
plot_population <- fPlot_PUsValues(PUs, "POP", logarithmic = TRUE, scale_fill = "magma")
ggsave(filename = "Figures/Population_log.svg", plot = plot_population, width = 49, height = 25)

#Plot of carbon stored
plot_carbon <- fPlot_PUsValues(PUs, "Tot_Carbon", logarithmic = FALSE, scale_fill = "viridis")
ggsave(filename = "Figures/Carbon.svg", plot = plot_carbon, width = 49, height = 25)

#Aggregate at 40000 km2 

#Plot of fishing intensity
plot_fish <- fPlot_PUsValues(PUs, "Fishing_Intensity", logarithmic = TRUE, large_PUs = TRUE, scale_fill = "mako")
ggsave(filename = "Figures/Fishing_Intensity_log_40000.svg", plot = plot_fish, width = 13, height = 6, dpi = 1000, units = "cm")

#Plot of properites protected
plot_properties <- fPlot_PUsValues(PUs, "TOT_STOCK", logarithmic = TRUE, large_PUs = TRUE, scale_fill = "cividis")
ggsave(filename = "Figures/Properties_log_40000.svg", plot = plot_properties, width = 13, height = 6, dpi = 1000, units = "cm")

#Plot of population protected
plot_population <- fPlot_PUsValues(PUs, "POP", logarithmic = TRUE, large_PUs = TRUE, scale_fill = "magma")
ggsave(filename = "Figures/Population_log_40000.svg", plot = plot_population, width = 13, height = 6, dpi = 1000, units = "cm")

#Plot of carbon stored
plot_carbon <- fPlot_PUsValues(PUs, "Tot_Carbon", logarithmic = FALSE, large_PUs = TRUE, scale_fill = "viridis")
ggsave(filename = "Figures/Carbon_40000.svg", plot = plot_carbon, width = 13, height = 6, dpi = 1000, units = "cm")

#Calculate number of species
PUs_Species <- PUs_NotSplitted %>%
  dplyr::select(names(species)) %>%
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(across(everything(), ~ifelse(. == 0, 0, 1))) %>% 
  mutate(tot = rowSums(.)) %>% 
  dplyr::select(tot)

PUs_Species <- PUs_NotSplitted %>% 
  cbind(PUs_Species)

#Plot number of species per planning unit
plot_species <- fPlot_PUsValues(PUs_Species, "tot", scale_fill = "inferno")
ggsave(filename = "Figures/Species.pdf", plot = plot_species, width = 49, height = 25)

#Plot number of species per planning unit
plot_species <- fPlot_PUsValues(PUs_Species, "tot", logarithmic = FALSE, large_PUs = TRUE, scale_fill = "inferno")
ggsave(filename = "Figures/Species_40000.svg", plot = plot_species, width = 13, height = 6, dpi = 1000, units = "cm")

#Distribution of mangroves
world_map <- ne_countries(scale = "large", returnclass = "sf") %>%
   st_transform(crs = cCRS) %>% #I project the crs of the GMW to meters
   st_make_valid() #I make the shapefile valid

Plot_GMW <- ggplot() +
  geom_sf(data = world_map, colour ="grey50", fill = "grey70", size = 0.1) +
  geom_sf(data = PUs, fill = "#006400", colour = "#006400", size = 0.1) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(plot = Plot_GMW, "Figures/GMW.svg",
       dpi = 300, width = 15, height = 8, units = "cm", limitsize = FALSE)

#Plot WDPA distribution
All_WDPA <- readRDS("RDS/WDPA_polygon_points_123_clean.rds") %>% 
  st_transform(cCRS) %>%
  st_make_valid()

FJI <- readRDS("RDS/wdpa_FJI.rds") %>% 
  st_transform(cCRS) %>%
  st_make_valid()

All_WDPA <- plyr::rbind.fill(All_WDPA, FJI) %>% 
  st_as_sf() %>%
  st_transform(cCRS) %>%
  st_make_valid()

WDPA <- All_WDPA %>% 
  filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV"))

#Plot the PAs map
Plot_WDPA <- ggplot() +
  geom_sf(data = world_map, colour ="grey50", fill = "grey70", size = 0.1) +
  geom_sf(data = WDPA, fill = "red", colour = "red", size = 0.1) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_rect(fill = "transparent"),
        panel.background = element_blank())

ggsave(plot = Plot_WDPA, "Figures/gurobi/WDPA.png", 
       dpi = 1000, width = 17, height = 10, units = "cm", limitsize = FALSE)

#Plot the PAs map
Plot_AllWDPA <- ggplot() +
  geom_sf(data = world_map, colour ="grey50", fill = "grey70", size = 0.1) +
  geom_sf(data = All_WDPA, fill = "#004D40", colour = "#004D40", size = 0.1) +
  geom_sf(data = WDPA, fill = "#FFC107", colour = "#FFC107", size = 0.1) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_rect(fill = "transparent"),
        panel.background = element_blank())

ggsave(plot = Plot_AllWDPA, "Figures/gurobi/AllWDPA.png", 
       dpi = 1000, width = 17, height = 10, units = "cm", limitsize = FALSE)


# Plot_WDPA <- ggplot() +
#   geom_sf(data = world_map, colour ="grey50", fill = "grey70", size = 0.1) +
#   geom_sf(data = PUs, aes(fill = Protected), colour = "NA", size = 0.1) +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         rect = element_rect(fill = "transparent"))