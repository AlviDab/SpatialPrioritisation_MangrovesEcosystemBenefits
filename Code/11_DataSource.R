#Author:Alvise Dabalà

#Code to prepare the source data excel
################################################################################
pacman::p_load(tidyverse, openxlsx)

Fig1 <- readRDS("RDS/Fig1.rds") %>% 
  filter(target != "100%")
Fig2 <- readRDS("RDS/Fig2.rds") 
Fig3 <- readRDS("RDS/Fig3.rds")
Fig4 <- readRDS("RDS/Fig4.rds")
SuppFig3 <- readRDS("RDS/SuppFig3.rds") %>% 
  filter(target != "100%")
SuppFig4 <- readRDS("RDS/SuppFig4.rds") %>% 
  filter(target != "100%")
SuppFig7 <- readRDS("RDS/SuppFig7.rds")
SuppFig8 <- readRDS("RDS/SuppFig8.rds")
SuppFig10 <- readRDS("RDS/SuppFig10.rds")
SuppFig11 <- readRDS("RDS/SuppFig11.rds")

list_figs <- list(Fig1, Fig2) %>% 
  append(Fig3) %>% 
  append(list(Fig4, SuppFig3, SuppFig4, SuppFig7, SuppFig8)) %>% 
  append(SuppFig10) %>% 
  append(list(SuppFig11))

names(list_figs)

list_sheets <- c("Fig1", "Fig2", "Fig3A", "Fig3B", "Fig3C", "Fig3D", "Fig3E", "Fig3F", "Fig4", 
                 "SuppFig3", "SuppFig4", "SuppFig7", "SuppFig8", "SuppFig10A", "SuppFig10B", 
                 "SuppFig10C", "SuppFig10D", "SuppFig10E", "SuppFig10F", "SuppFig11")

names(list_figs) <- list_sheets

openxlsx::write.xlsx(list_figs, paste0(file, "SourceData.xlsx"))
