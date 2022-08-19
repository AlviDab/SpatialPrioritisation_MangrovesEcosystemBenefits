#Author: Alvise Dabalà

#Code to produce the barplot included in the study

#Open all the packages needed
library(tidyverse)
library(sf)
library(prioritizr)
library(patchwork)
library(viridis)
library(ggthemes)
library(openxlsx)
library(xlsx)

################################################################################
# Barplots 

# Create a tibble with the values of representation for 30, and 50% area budget
source("Functions/fPlot_BarPlots.r")

PUs <- readRDS("RDS/PUs_Splitted_I_IV.rds")
result_BioServ <- readRDS("RDS/result_BioServ.rds")
result_BioServ_WDPA <- readRDS("RDS/result_BioServ_WDPA.rds")
result_Bio <- readRDS("RDS/result_Bio.rds")
result_Bio_WDPA <- readRDS("RDS/result_Bio_WDPA.rds")

#Solutions 30% and 50%
sol_ConsFeat_30_WDPA <- result_BioServ_WDPA %>% 
  filter(rank <= 30) %>% 
  filter(Protected != 1)

sol_ConsFeat_30_Bio_WDPA <- result_Bio_WDPA %>% 
  filter(rank <= 30) %>% 
  filter(Protected != 1)

sol_ConsFeat_50_WDPA <- result_BioServ_WDPA %>% 
  filter(rank > 30 & rank <= 50) %>% 
  filter(Protected != 1)

sol_ConsFeat_50_Bio_WDPA <- result_Bio_WDPA %>% 
  filter(rank > 30 & rank <= 50) %>% 
  filter(Protected != 1)

# Barplot
Plot30_WDPA <- fplot_BarPlots(PUs, sol_ConsFeat_30_WDPA, sol_ConsFeat_30_Bio_WDPA, "Figures/Barplot_WDPA30")
Plot50_WDPA <- fplot_BarPlots(PUs, sol_ConsFeat_50_WDPA, sol_ConsFeat_50_Bio_WDPA, "Figures/Barplot_WDPA50")

Plot30_WDPA[[2]] + Plot50_WDPA[[2]] + Plot30_WDPA[[3]] + Plot50_WDPA[[3]] +
  Plot30_WDPA[[4]] + Plot50_WDPA[[4]] + Plot30_WDPA[[1]] + Plot50_WDPA[[1]] +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a') +
  theme(plot.tag = element_text(face = 'bold'))

ggsave("Figures/Barplot.svg", width = 17.0, height = 22, units = "cm")

