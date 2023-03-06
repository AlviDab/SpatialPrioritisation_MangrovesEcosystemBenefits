#Author: Alvise Dabalà
#Date: 2023/03/06

#Supplementary analysis to improve the manuscript
################################################################################
#Plot correlation matrix using kappa index
result_BioServ <- readRDS("RDS_rr/1e-4/gurobi/result_BioServ.rds")
result_Bio <- readRDS("RDS_rr/1e-4/gurobi/result_Bio.rds")
result_BioServ_WDPA <- readRDS("RDS_rr/1e-4/gurobi/result_Bio_WDPA.rds")
result_Bio_WDPA <- readRDS("RDS_rr/1e-4/gurobi/result_Bio_WDPA.rds")

source("Functions/fCreate_Kappa.R")

#produce result column for rank <= 30
result_BioServ <- result_BioServ %>% 
  mutate(solution_1 = case_when(rank <= 30 ~ TRUE,
                          rank > 30 ~ FALSE))

result_Bio <- result_Bio %>% 
  mutate(solution_1 = case_when(rank <= 30 ~ TRUE,
                                rank > 30 ~ FALSE))

result_BioServ_WDPA <- result_BioServ_WDPA %>% 
  mutate(solution_1 = case_when(rank <= 30 ~ TRUE,
                                rank > 30 ~ FALSE))

result_Bio_WDPA <- result_Bio_WDPA %>% 
  mutate(solution_1 = case_when(rank <= 30 ~ TRUE,
                                rank > 30 ~ FALSE))

Kappa <- fcreate_kappacorrplot(list(result_BioServ, result_Bio, result_BioServ_WDPA, result_Bio_WDPA), c("Biodiversity and ecosystem services", 
                                                                                                         "Biodiversity", 
                                                                                                         "Protected areas, biodiversity and ecosystem services",
                                                                                                         "Protected areas and biodiversity"))

png("Figures_rr/gurobi/Kappa.png", width = 30, height = 25, units = "cm", res = 300)

Kappa$plot()

dev.off()

################################################################################
#Target reached PAs by species
PUs <- readRDS("RDS_rr/PUs_Splitted_I_IV_and_All_9111.rds")

#WDPA I-IV
targets_reached_PAs <- PUs %>%
  dplyr::select(contains(c(names(species))), Protected) %>% #Select only species col and protected column
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(Protected) %>% #Group if protected or not
  summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop') %>% #sum each column value by group
  pivot_longer(!Protected, names_to = "names", values_to = "Area") %>% #Transform to long format, names are the name of each column, values are from the amount of area protected or not
  pivot_wider(names_from = Protected, values_from = "Area") %>% #Transform to wide format the area column from values of protected or not protected 
  left_join(ConsFeatures, by = "names") #Add the minimum amount of service 

targets_reached_PAs_spec <- targets_reached_PAs %>% 
  #mutate(names = gsub("_.*","", targets_reached_PAs$names)) %>% 
  mutate(amount_protected = `TRUE`/(`FALSE` + `TRUE`)) %>% #Percentage of protected and not protected area by conservation feature
  mutate(protected = case_when(amount_protected < amount ~ 0, #When amount protected < target the result is 0
                               TRUE ~ 1))

targets_reached_PAs_spec$spec <- sub("\\_.*", "", targets_reached_PAs_spec$names)

targets_reached_PAs_spec <- targets_reached_PAs_spec %>% 
  group_by(spec) %>% 
  summarise(protected = case_when(n() == sum(protected) ~ 1,
                                  n() > sum(protected) ~ 0
                                  ))
  
