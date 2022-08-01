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

################################################################################
# Kernel's plot

# Create a tibble with the values of representation for 30, and 50% area budget
source("Functions/fPlot_Kernel.r")

result_BioServ <- readRDS("RDS/result_BioServ.rds")
result_BioServ_WDPA <- readRDS("RDS/result_BioServ_WDPA.rds")
result_Bio <- readRDS("RDS/result_Bio.rds")
result_Bio_WDPA <- readRDS("RDS/result_Bio_WDPA.rds")

#Solutions 30% and 50%
sol_ConsFeat_30_WDPA <- result_BioServ_WDPA %>% 
  filter(rank %in% c(28, 29, 30)) %>% 
  filter(Protected != 1)

sol_ConsFeat_30_Bio_WDPA <- result_Bio_WDPA %>% 
  filter(rank %in% c(28, 29, 30)) %>% 
  filter(Protected != 1)

sol_ConsFeat_50_WDPA <- result_BioServ_WDPA %>% 
  filter(rank >= 30 & rank <= 50) %>% 
  filter(Protected != 1)

sol_ConsFeat_50_Bio_WDPA <- result_Bio_WDPA %>% 
  filter(rank >= 30 & rank <= 50) %>% 
  filter(Protected != 1)

# Kernel plot
fplot_KernelPlots(PUs, sol_ConsFeat_30_WDPA, sol_ConsFeat_30_Bio_WDPA, "Figures/Kernel_WDPA30")
fplot_KernelPlots(PUs, sol_ConsFeat_50_WDPA, sol_ConsFeat_50_Bio_WDPA, "Figures/Kernel_WDPA50")