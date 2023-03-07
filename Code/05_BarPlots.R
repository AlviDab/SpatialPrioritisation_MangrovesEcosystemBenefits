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

PUs <- readRDS("RDS/PUs_Splitted_I_IV_and_All_9111.rds")
result_BioServ <- readRDS("RDS_rr/1e-4/gurobi/result_BioServ.rds")
result_BioServ_WDPA <- readRDS("RDS_rr/1e-4/gurobi/result_BioServ_WDPA.rds")
result_Bio <- readRDS("RDS_rr/1e-4/gurobi/result_Bio.rds")
result_Bio_WDPA <- readRDS("RDS_rr/1e-4/gurobi/result_Bio_WDPA.rds")
result_BioServ_AllWDPA <- readRDS("RDS_rr/1e-4/gurobi/result_BioServ_AllWDPA.rds")
result_BioServ_WDPA <- readRDS("RDS_rr/1e-4/gurobi/result_BioServ_WDPA.rds")

#Solutions 30% and 50%
sol_ConsFeat_30_WDPA <- result_BioServ_WDPA %>% 
  filter(rank <= 30) %>% #Select results with rank <30
  filter(Protected == FALSE) #Filter the PUs that are not protected

sol_ConsFeat_30_Bio_WDPA <- result_Bio_WDPA %>% 
  filter(rank <= 30) %>% #Select results with rank <30
  filter(Protected == FALSE) #Filter the PUs that are not protected

sol_ConsFeat_50_WDPA <- result_BioServ_WDPA %>% 
  filter(rank > 30 & rank <= 50) %>% #select results with rank >30 and <=50
  filter(Protected == FALSE)

sol_ConsFeat_50_Bio_WDPA <- result_Bio_WDPA %>% 
  filter(rank > 30 & rank <= 50) %>% #select results with rank >30 and <=50
  filter(Protected == FALSE)

# Barplot
Plot30_WDPA <- fplot_BarPlots(PUs, sol_ConsFeat_30_WDPA, sol_ConsFeat_30_Bio_WDPA)
Plot50_WDPA <- fplot_BarPlots(PUs, sol_ConsFeat_50_WDPA, sol_ConsFeat_50_Bio_WDPA)

Plot30_WDPA[[2]] + #Plot50_WDPA[[2]] + 
  Plot30_WDPA[[3]] + #Plot50_WDPA[[3]] +
  Plot30_WDPA[[4]] + #Plot50_WDPA[[4]] + 
  Plot30_WDPA[[1]] + #Plot50_WDPA[[1]] +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a') +
  theme(plot.tag = element_text(face = 'bold'))

ggsave("Figures_rr/gurobi/Barplot.svg", width = 17.0, height = 12, units = "cm")
