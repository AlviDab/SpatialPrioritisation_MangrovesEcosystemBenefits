#Author: Alvise Dabalà

#Code to produce the priority areas plots at country scale

#Open all the packages needed
library(tidyverse)
library(sf)
library(prioritizr)
library(patchwork)
library(viridis)
library(ggthemes)
library(rnaturalearth)

#Here I produce the graphs that report the values of the ecosystem benefits and
#biodiversity benefits for each planning unit

cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

source("Functions/fPlot_Rank.R")

result_BioServ <- readRDS("RDS/result_BioServ.rds")
result_BioServ_WDPA <- readRDS("RDS/result_BioServ_WDPA.rds")
species <- readRDS("RDS/species.rds")
PUs <- readRDS("RDS/PUs_Splitted.rds")
PUs_NotSplitted <- readRDS("RDS/PUs_NotSplitted.rds")
Large_PUs <- readRDS("RDS/Large_PUs_40000.rds")
species <- readRDS("RDS/species.rds")
result_BioServ_WDPA_rmPA <- readRDS("RDS/result_BioServ_WDPA_rmPA.rds")
#Maps zoom

#Thailand Vietnam
limits <- st_bbox(c(xmin = 96.7, xmax = 108.2, ymin = 4, ymax = 22.5), crs = st_crs(4326)) %>% 
  st_as_sfc() %>%
  st_transform(cCRS) %>%
  st_bbox()

plot <- fPlot_Rank(result_BioServ, limits = limits, palet = "viridis")

ggsave("Figures/ThaVnm.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(14, 25, 50, 75, 100), lm = c(14, 100))

ggsave("Figures/ThaVnm_WDPA.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

#PNG
limits <- st_bbox(c(xmin = 140, xmax = 152.5, ymin = -18.5, ymax = 1), crs = st_crs(4326)) %>% 
  st_as_sfc() %>%
  st_transform(cCRS) %>%
  st_bbox()

plot <- fPlot_Rank(result_BioServ, limits = limits, palet = "viridis")

ggsave("Figures/PNG.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(14, 50, 75, 100), lm = c(14, 100))

ggsave("Figures/PNG_WDPA.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

#SE Asia
limits <- st_bbox(c(xmin = 108, xmax = 125.5, ymin = -0.3, ymax = -11.5), crs = st_crs(4326)) %>% 
  st_as_sfc() %>%
  st_transform(cCRS) %>%
  st_bbox()

plot <- fPlot_Rank(result_BioServ, limits = limits, palet = "viridis")

ggsave("Figures/Zoom/SEAsia.pdf",  dpi = 1000, 
       width = 8, height = 6, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(14, 25, 50, 75, 100), lm = c(14, 100))

ggsave("Figures/SEAsia_WDPA.pdf",  dpi = 1000, 
       width = 8, height = 6, units = "cm", limitsize = FALSE)


#Red Sea 
limits <- st_bbox(c(xmin = 44, xmax = 32.5, ymin = 12, ymax = 29.5), crs = st_crs(4326)) %>% 
  st_as_sfc() %>%
  st_transform(cCRS) %>%
  st_bbox()

plot <- fPlot_Rank(result_BioServ, limits = limits, palet = "viridis")

ggsave("Figures/RedSea.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(14, 25, 50, 75, 100), lm = c(14, 100))

ggsave("Figures/RedSea_WDPA.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

#Latin America
limits <- st_bbox(c(xmin = -93.5, xmax = -76, ymin = 11.5, ymax = 24), crs = st_crs(4326)) %>% 
  st_as_sfc() %>%
  st_transform(cCRS) %>%
  st_bbox()

plot <- fPlot_Rank(result_BioServ, limits = limits, palet = "viridis")

ggsave("Figures/LatAm.pdf",  dpi = 1000, 
       width = 8, height = 6, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(14, 25, 50, 75, 100), lm = c(14, 100))

ggsave("Figures/LatAm_WDPA.pdf",  dpi = 1000, 
       width = 8, height = 6, units = "cm", limitsize = FALSE)
