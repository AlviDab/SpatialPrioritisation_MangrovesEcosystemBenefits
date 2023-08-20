#Author: Alvise Dabalà

#Code to produce the priority areas plots at country scale

#Open all the packages needed
pacman::p_load(tidyverse, sf, prioritizr, patchwork, viridis, ggthemes, rnaturalearth)

#Here I produce the graphs that report the values of the ecosystem benefits and
#biodiversity benefits for each planning unit

cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

source("Functions/fPlot_Rank.R")

result_BioServ <- readRDS("RDS/1e-4/gurobi/result_BioServ.rds")
result_BioServ_WDPA <- readRDS("RDS/1e-4/gurobi/result_BioServ_WDPA.rds")
result_BioServ_AllWDPA <- readRDS("RDS/1e-4/gurobi/result_BioServ_AllWDPA.rds")
PUs <- readRDS("RDS/PUs_Splitted_I_IV_and_All_9111.rds")
Large_PUs <- readRDS("RDS/Large_PUs.rds")
result_BioServ_WDPA_rmPA <- readRDS("RDS/1e-4/gurobi/result_BioServ_WDPA_rmPA.rds")
result_BioServ_AllWDPA_rmPA <- readRDS("RDS/1e-4/gurobi/result_BioServ_AllWDPA_rmPA.rds")

#Maps zoom

#Thailand Vietnam
limits <- st_bbox(c(xmin = 96.7, xmax = 108.2, ymin = 4, ymax = 22.5), crs = st_crs(4326)) %>% 
  st_as_sfc() %>%
  st_transform(cCRS) %>%
  st_bbox()

plot <- fPlot_Rank(result_BioServ, limits = limits, palet = "viridis")

ggsave("Figures/gurobi/ThaVnm.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(14, 25, 50, 75, 100), lm = c(14, 100))

ggsave("Figures/gurobi/ThaVnm_WDPA.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_AllWDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(44, 50, 75, 100), lm = c(44, 100))

ggsave("Figures/gurobi/ThaVnm_AllWDPA.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

#PNG
limits <- st_bbox(c(xmin = 140, xmax = 152.5, ymin = -18.5, ymax = 1), crs = st_crs(4326)) %>% 
  st_as_sfc() %>%
  st_transform(cCRS) %>%
  st_bbox()

plot <- fPlot_Rank(result_BioServ, limits = limits, palet = "viridis")

ggsave("Figures/gurobi/PNG.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(14, 50, 75, 100), lm = c(14, 100))

ggsave("Figures/gurobi/PNG_WDPA.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(14, 25, 50, 75, 100), lm = c(14, 100))

ggsave("Figures/gurobi/PNG_WDPA.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_AllWDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(44, 50, 75, 100), lm = c(44, 100))

ggsave("Figures/gurobi/PNG_AllWDPA.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)


#SE Asia
limits <- st_bbox(c(xmin = 108, xmax = 125.5, ymin = -0.3, ymax = -11.5), crs = st_crs(4326)) %>% 
  st_as_sfc() %>%
  st_transform(cCRS) %>%
  st_bbox()

plot <- fPlot_Rank(result_BioServ, limits = limits, palet = "viridis")

ggsave("Figures/gurobi/SEAsia.pdf",  dpi = 1000, 
       width = 8, height = 6, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(14, 25, 50, 75, 100), lm = c(14, 100))

ggsave("Figures/gurobi/SEAsia_WDPA.pdf",  dpi = 1000, 
       width = 8, height = 6, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_AllWDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(44, 50, 75, 100), lm = c(44, 100))

ggsave("Figures/gurobi/SEAsia_AllWDPA.pdf",  dpi = 1000, 
       width = 8, height = 6, units = "cm", limitsize = FALSE)

#Red Sea 
limits <- st_bbox(c(xmin = 44, xmax = 32.5, ymin = 12, ymax = 29.5), crs = st_crs(4326)) %>% 
  st_as_sfc() %>%
  st_transform(cCRS) %>%
  st_bbox()

plot <- fPlot_Rank(result_BioServ, limits = limits, palet = "viridis")

ggsave("Figures/gurobi/RedSea.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(14, 25, 50, 75, 100), lm = c(14, 100))

ggsave("Figures/gurobi/RedSea_WDPA.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_AllWDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(44, 50, 75, 100), lm = c(44, 100))

ggsave("Figures/gurobi/RedSea_AllWDPA.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)


#Latin America
limits <- st_bbox(c(xmin = -93.5, xmax = -76, ymin = 11.5, ymax = 24), crs = st_crs(4326)) %>% 
  st_as_sfc() %>%
  st_transform(cCRS) %>%
  st_bbox()

plot <- fPlot_Rank(result_BioServ, limits = limits, palet = "viridis")

ggsave("Figures/gurobi/LatAm.pdf",  dpi = 1000, 
       width = 8, height = 6, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(14, 25, 50, 75, 100), lm = c(14, 100))

ggsave("Figures/gurobi/LatAm_WDPA.pdf",  dpi = 1000, 
       width = 8, height = 6, units = "cm", limitsize = FALSE)


plot <- fPlot_Rank(result_BioServ_AllWDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(44, 50, 75, 100), lm = c(44, 100))

ggsave("Figures/gurobi/LatAm_AllWDPA.pdf",  dpi = 1000, 
       width = 8, height = 6, units = "cm", limitsize = FALSE)
