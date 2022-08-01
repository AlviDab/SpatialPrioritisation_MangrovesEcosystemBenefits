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

############################################
#I set the various layer for the analysis

#Prepare the files

#Source the different function
source("Functions/fPUs_GMW.R") #Function to produce the PUs form the shapefile of the mangroves distribution (Thanks to Jason Everett)
source("Functions/fIntersection_IUCNnearestfeature.R") #Function to intersect the PUs with the species distribution
source("Functions/fSelect_LockedIn.R") #Function to select the PUs that are locked-in because already protected
source("Functions/fSelect_PUsArea.R") #Function to select the area of the PUs
source("Functions/fExtract_CarbonSequestration.R") #Function to select the area of the PUs
source("Functions/fCalculate_BioTypArea.R") #Function to select the area of the PUs
source("Functions/fCompareSolutions.R")
source("Functions/fCompareThreeSolutions.R")
source("Functions/fPlot_PrioritizrSolution.R")
source("Functions/fSolve_Prioritizr.R")
source("Functions/fPlot_GlobalResults.R")
source("Functions/fIntersect_CoastalSqueeze.R")
source("Functions/fIntersect_PointShp.R")
source("Functions/fRemove_NANearestNeighbourg.R")
source("Functions/fCreate_KappacorrplotFourScenarios.R")
source("Functions/fSelect_WDPA.R")
source("Functions/fPlot_Kernel.R")
source("Functions/fPlot_PUsValues.R")
source("Functions/fPlot_Rank.R")
source("Functions/f_PlotCircular.R")
source("Functions/fPlot_Radar.R")

#Set the projection
cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"


################################################################################

#Open PUs and large_PUs

PUs <- readRDS("RDS/PUs_SplittedSpecies.rds")
PUs_NotSplitted <- readRDS("RDS/PUs_NotSplitted.rds")
Large_PUs <- readRDS("RDS/Large_PUs_40000.rds")

#Scaling
library(scales)
library(openxlsx)

#I transform all the values to numeric
PUs <- PUs %>% 
  mutate(across(!geometry & !Protected & !country & !continent, 
                as.numeric))

#Calculate total benefits
sum_benefits <- PUs %>%
  st_drop_geometry() %>% 
  tibble() %>% 
  dplyr::select(Fishing_Intensity,
                TOT_STOCK,
                POP,
                Tot_Carbon) %>% 
  summarise(across(everything(), ~ sum(.)))

#Rescale the benefits to avoid too large or too small values
PUs <- PUs %>%
  mutate(Fishing_Intensity_prc = case_when(
    Fishing_Intensity == 0 ~ 0,
    TRUE ~ (scales::rescale(Fishing_Intensity*AreaGMWKm, c(1, 1000)))),
    TOT_STOCK_prc = case_when(
      TOT_STOCK == 0 ~ 0,
      TRUE ~ (scales::rescale(TOT_STOCK*AreaGMWKm, c(1, 1000)))),
    POP_prc = case_when(
      POP == 0 ~ 0,
      TRUE ~ (scales::rescale(POP*AreaGMWKm, c(1, 1000)))),
    Tot_Carbon_prc = case_when(
      Tot_Carbon == 0 ~ 0,
      TRUE ~ (scales::rescale(Tot_Carbon*AreaGMWKm, c(1, 1000)))))

################################################################################
#Targets 

#Calculate species distribution range
species <- PUs_NotSplitted %>% 
  as_tibble() %>% 
  dplyr::select(3:67)

species <- species %>% 
  summarise_all(sum)

saveRDS(species, "RDS/species.rds")

#Calculate species targets following Rodrigues et al. 2014 and Butchart et al. 2015
spp_range_size_km2 <- seq(0.01, max(species), by = 100)

#we can now use this function to calculate representation targets
#(expressed as a percentage of the species' range sizes) using
#the thresholds and cap sizes reported by Rodrigues et al. 2004
spp_target_percentage_rodrigues <-
  loglinear_interpolation(
    x = spp_range_size_km2,
    coordinate_one_x = 1000,
    coordinate_one_y = 1,
    coordinate_two_x = 250000,
    coordinate_two_y = 0.1) * 100

#it is also common to apply a cap to the representation targets,
#so let's apply the cap these targets following Butchart et al. (2015)
spp_target_percentage_butchart <- ifelse(
  spp_range_size_km2 >= 10000000,
  (1000000 / spp_range_size_km2) * 100,
  spp_target_percentage_rodrigues)

# Select the target for each feature
targets_species <- lapply(species, function(x) {
  a <- spp_range_size_km2 %>% 
    as_tibble() %>% 
    mutate(ID = rownames(.)) %>% 
    slice(which.min(abs(value - x))) %>% 
    dplyr::select(ID) %>% 
    as.numeric()
  
  spp_target_percentage_butchart[[a[[1]]]]
}
) %>%  
  unname() %>% 
  unlist()

targets_species <- tibble(names = as.character(names(species)), amount = as.numeric(targets_species)/100, w = 1)

#Select conservation features columns
ConsFeatures <- PUs %>% 
  st_drop_geometry() %>% 
  dplyr::select(contains(c(names(species))),
                "Tot_Carbon_prc", "Fishing_Intensity_prc", "TOT_STOCK_prc", "POP_prc") %>%  
  summarise(names = as.character(colnames(.)), amount = 0.3)

ConsFeatures_NotSplitted <- PUs_NotSplitted %>% 
  st_drop_geometry() %>% 
  dplyr::select(contains(c(names(species)))) %>%  
  summarise(names = as.character(colnames(.)), amount = 0.3)

#Include the weights
ConsFeatures <- ConsFeatures %>% 
  mutate(w = 1) %>% 
  mutate(amount = ifelse(names %in% c("Tot_Carbon_prc", "Fishing_Intensity_prc", "TOT_STOCK_prc", "POP_prc"), 1, 0.3))

for(i in 1:length(species)) {
  #Add species targets
  
  ConsFeatures <- ConsFeatures %>% 
    mutate(amount = case_when(
      str_detect(ConsFeatures$names, targets_species$names[i]) == TRUE ~ targets_species$amount[i],
                   TRUE ~ amount))
  
  ConsFeatures_NotSplitted <- ConsFeatures_NotSplitted %>% 
    mutate(amount = case_when(
      str_detect(ConsFeatures_NotSplitted$names, targets_species$names[i]) == TRUE ~ targets_species$amount[i],
      TRUE ~ amount))
      
}


#Define the type
ConsFeatures <- ConsFeatures %>%
  mutate(type = case_when(amount < 1 ~ "Species",
                          TRUE ~ "EcoServices"))

saveRDS(ConsFeatures, "RDS/ConsFeatures.rds")
saveRDS(ConsFeatures_NotSplitted, "RDS/ConsFeatures_NotSplitted.rds")

################################################################################
# Optimisation biodiversity and ecosystem services
################################################################################

list_sol_AreaTarget_BioServ <- list()

startTime <- Sys.time()

PUs$LockedIn <- FALSE

for(x in 1:100) {
  
  if(x == 1) {
    p_AreaTarget <- problem(PUs, features = ConsFeatures$names, cost_column = "AreaGMWKm") %>% #Area Target
      add_min_shortfall_objective(sum(PUs$AreaGMWKm*(x/100))) %>% #Maximum cost is 30% of the total area
      add_relative_targets(ConsFeatures$amount) %>% # representation targets (Area)
      add_binary_decisions() %>%
      add_gurobi_solver(gap = 1e-4, threads = 8) %>% 
      #add_rsymphony_solver(verbose = FALSE) %>% 
      add_feature_weights(ConsFeatures$w)
  }
  
  if(x != 1) {
    PUs$LockedIn <- as.logical(sol_AreaTarget$solution_1)
    
    p_AreaTarget <- problem(PUs, features = ConsFeatures$names, cost_column = "AreaGMWKm") %>% #Area Target
      add_min_shortfall_objective(sum(PUs$AreaGMWKm*(x/100))) %>% #Maximum cost is 30% of the total area
      add_relative_targets(ConsFeatures$amount) %>% # representation targets (Area)
      add_locked_in_constraints(locked_in = "LockedIn") %>% 
      add_binary_decisions() %>%
      add_gurobi_solver(gap = 1e-4, threads = 8) %>% 
      #add_rsymphony_solver(verbose = FALSE) %>% 
      add_feature_weights(ConsFeatures$w)
  }
  
  sol_AreaTarget <- solve(p_AreaTarget)
  
  list_sol_AreaTarget_BioServ[[x]] <- list(p_AreaTarget, sol_AreaTarget)
}

endTime <- Sys.time()

# prints recorded time
print(endTime - startTime)

### Calculate resulting shapefile
result_BioServ <- PUs %>% 
  dplyr::select(ID, Protected)

for(i in 1:length(list_sol_AreaTarget_BioServ)) {
  sol <- list_sol_AreaTarget_BioServ[[i]][[2]] %>% 
    dplyr::select(solution_1, ID) %>% 
    st_drop_geometry() %>% 
    tibble()
  
  result_BioServ <- result_BioServ %>%
    left_join(sol, by = "ID")
}

result_BioServ <- result_BioServ %>%  
  tibble() %>%
  summarise(rank = rowSums(dplyr::select(., (!ID & !geometry & !Protected))), 
            geometry = geometry, 
            Protected = Protected,
            ID = ID) %>% 
  st_as_sf()

result_BioServ$rank <- -1*(result_BioServ$rank - 1) + 100

# Priority areas by country
result_BioServ <- result_BioServ %>%
  as_tibble %>% 
  dplyr::select(rank, ID) %>%
  left_join(PUs, by = 'ID')

result_BioServ <- result_BioServ %>%
  st_as_sf

# Save the resulting shapefile
saveRDS(result_BioServ, paste0("RDS/result_BioServ.rds"))

# World map
plot_global_map <- fPlot_Rank(result_BioServ, Large_PUs, palet = "viridis") 
ggsave(plot = plot_global_map, paste0("Figures/Rank_Global_40000.svg"),
       dpi = 1000, width = 18, height = 9, units = "cm", limitsize = FALSE)

plot_results <- fPlot_PUsValues(result_BioServ, "rank", scale_fill = "viridis") 
ggsave(plot = plot_results, paste0("Figures/Rank_Global.svg"),
       dpi = 1000, width = 18, height = 9, units = "cm", limitsize = FALSE)

################################################################################
# Optimisation biodiversity and ecosystem services building on already protected areas
################################################################################

list_sol_AreaTarget_BioServ_WDPA <- list()

startTime <- Sys.time()

PUs$LockedIn <- PUs$Protected

for(x in 28:100) {
  
  if(x != 28) {PUs$LockedIn <- as.logical(sol_AreaTarget$solution_1)} else
  {PUs$LockedIn <- PUs$Protected}
  
  p_AreaTarget <- problem(PUs, features = ConsFeatures$names, cost_column = "AreaGMWKm") %>% #Area Target
    add_min_shortfall_objective(sum(PUs$AreaGMWKm*(x/100))) %>% #Maximum cost is 30% of the total area
    add_relative_targets(ConsFeatures$amount) %>% # representation targets (Area)
    add_locked_in_constraints(locked_in = "LockedIn") %>% 
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 1e-4, threads = 8) %>% 
    #add_rsymphony_solver(verbose = FALSE) %>% 
    add_feature_weights(ConsFeatures$w)
  
  sol_AreaTarget <- solve(p_AreaTarget)
  
  list_sol_AreaTarget_BioServ_WDPA[[x-27]] <- list(p_AreaTarget, sol_AreaTarget)
}

endTime <- Sys.time()

# prints recorded time
print(endTime - startTime)

### Calculate resulting shapefile
result_BioServ_WDPA <- PUs %>% 
  dplyr::select(ID, Protected)

for(i in 1:length(list_sol_AreaTarget_BioServ_WDPA)) {
  sol <- list_sol_AreaTarget_BioServ_WDPA[[i]][[2]] %>% 
    dplyr::select(solution_1, ID) %>% 
    st_drop_geometry() %>% 
    tibble()
  
  result_BioServ_WDPA <- result_BioServ_WDPA %>%
    left_join(sol, by = "ID")
}

result_BioServ_WDPA <- result_BioServ_WDPA %>%  
  tibble() %>%
  summarise(rank = rowSums(dplyr::select(., (!ID & !geometry & !Protected))), 
            geometry = geometry, 
            Protected = Protected,
            ID = ID) %>% 
  st_as_sf()

result_BioServ_WDPA$rank <- -1*(result_BioServ_WDPA$rank - 1) + 100

# Priority areas by country
result_BioServ_WDPA <- result_BioServ_WDPA %>%
  as_tibble %>% 
  dplyr::select(rank, ID) %>%
  left_join(PUs, by = 'ID')

result_BioServ_WDPA_rmPA <- result_BioServ_WDPA %>% 
  filter(Protected == 0) %>% 
  st_as_sf

result_BioServ_WDPA <- result_BioServ_WDPA %>%
  st_as_sf

# Save the resulting shapefile
saveRDS(result_BioServ_WDPA, "RDS/result_BioServ_WDPA.rds")
saveRDS(result_BioServ_WDPA_rmPA, "RDS/result_BioServ_WDPA_rmPA.rds")

plot_global_map <- fPlot_Rank(result_BioServ_WDPA_rmPA, Large_PUs, palet = "viridis",
                              brk = c(28, 50, 75, 100), lm = c(28, 100)) 
ggsave(plot = plot_global_map, paste0("Figures/Rank_Global_40000_WDPA.pdf"),
       dpi = 1000, width = 18, height = 9, units = "cm", limitsize = FALSE)

plot_results <- fPlot_PUsValues(result_BioServ_WDPA_rmPA, "rank", scale_fill = "viridis") 
ggsave(plot = plot_results, paste0("Figures/Rank_Global_WDPA.svg"),
       dpi = 1000, width = 18, height = 9, units = "cm", limitsize = FALSE)

################################################################################
# Optimisation biodiversity
################################################################################

# Select conservation features
ConsFeatures <- ConsFeatures %>% 
  dplyr::filter(type != "EcoServices")

list_sol_AreaTarget_Bio <- list()

startTime <- Sys.time()

PUs$LockedIn <- FALSE

for(x in 1:100) {
  
  if(x == 1) {
    p_AreaTarget_Bio <- problem(PUs, features = ConsFeatures$names, cost_column = "AreaGMWKm") %>% #Area Target
      add_min_shortfall_objective(sum(PUs$AreaGMWKm*(x/100))) %>% #Maximum cost is 30% of the total area
      add_relative_targets(ConsFeatures$amount) %>% # representation targets (Area)
      add_binary_decisions() %>%
      add_gurobi_solver(gap = 1e-4, threads = 8) %>% 
      #add_rsymphony_solver(verbose = FALSE) %>% 
      add_feature_weights(ConsFeatures$w)
  }
  
  if(x != 1) {
    PUs$LockedIn <- as.logical(sol_AreaTarget_Bio$solution_1)
    
    p_AreaTarget_Bio <- problem(PUs, features = ConsFeatures$names, cost_column = "AreaGMWKm") %>% #Area Target
      add_min_shortfall_objective(sum(PUs$AreaGMWKm*(x/100))) %>% #Maximum cost is 30% of the total area
      add_relative_targets(ConsFeatures$amount) %>% # representation targets (Area)
      add_locked_in_constraints(locked_in = "LockedIn") %>% 
      add_binary_decisions() %>%
      add_gurobi_solver(gap = 1e-4, threads = 8) %>% 
      #add_rsymphony_solver(verbose = FALSE) %>% 
      add_feature_weights(ConsFeatures$w)
  }
  
  sol_AreaTarget_Bio <- solve(p_AreaTarget_Bio)
  
  list_sol_AreaTarget_Bio[[x]] <- list(p_AreaTarget_Bio, sol_AreaTarget_Bio)
}

endTime <- Sys.time()

# prints recorded time
print(endTime - startTime)

### Calculate resulting shapefile prioritisation
result_Bio <- PUs %>% 
  dplyr::select(ID, Protected)

for(i in 1:length(list_sol_AreaTarget_Bio)) {
  sol <- list_sol_AreaTarget_Bio[[i]][[2]] %>% 
    dplyr::select(solution_1, ID) %>% 
    st_drop_geometry() %>% 
    tibble()
  
  result_Bio <- result_Bio %>%
    left_join(sol, by = "ID")
}

result_Bio <- result_Bio %>%  
  tibble() %>%
  summarise(rank = rowSums(dplyr::select(., (!ID & !geometry & !Protected))), 
            geometry = geometry, 
            Protected = Protected,
            ID = ID) %>% 
  st_as_sf()

result_Bio$rank <- -1*(result_Bio$rank - 1) + 100

result_Bio <- result_Bio %>%
  as_tibble %>% 
  dplyr::select(rank, ID) %>%
  left_join(PUs, by = 'ID')

result_Bio <- result_Bio %>% 
  mutate(rank = case_when(
    rank == 101 ~ 100,
    TRUE ~ .$rank)) %>% 
  st_as_sf()

# Save the resulting shapefile
saveRDS(result_Bio, "RDS/result_Bio.rds")

plot_global_map <- fPlot_Rank(result_Bio, Large_PUs, palet = "viridis") 
ggsave(plot = plot_global_map, "Figures/Rank_Global_Bio_40000.svg",
       dpi = 1000, width = 18, height = 9, units = "cm", limitsize = FALSE)

plot_results <- fPlot_PUsValues(result_Bio, "rank", scale_fill = "viridis") 
ggsave(plot = plot_results, "Figures/Rank_Global_Bio.svg",
       dpi = 1000, width = 18, height = 9, units = "cm", limitsize = FALSE)

################################################################################
# Optimisation biodiversity building on already protected areas
################################################################################

list_sol_AreaTarget_Bio_WDPA <- list()

startTime <- Sys.time()

PUs$LockedIn <- PUs$Protected

for(x in 28:100) {
  
  if(x != 28) {PUs$LockedIn <- as.logical(sol_AreaTarget$solution_1)} else
  {PUs$LockedIn <- PUs$Protected}
  
  p_AreaTarget <- problem(PUs, features = ConsFeatures$names, cost_column = "AreaGMWKm") %>% #Area Target
    add_min_shortfall_objective(sum(PUs$AreaGMWKm*(x/100))) %>% #Maximum cost is 30% of the total area
    add_relative_targets(ConsFeatures$amount) %>% # representation targets (Area)
    add_locked_in_constraints(locked_in = "LockedIn") %>% 
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 1e-4, threads = 8) %>% 
    #add_rsymphony_solver(verbose = FALSE) %>% 
    add_feature_weights(ConsFeatures$w)
  
  sol_AreaTarget <- solve(p_AreaTarget)
  
  list_sol_AreaTarget_Bio_WDPA[[x-27]] <- list(p_AreaTarget, sol_AreaTarget)
}

endTime <- Sys.time()

# prints recorded time
print(endTime - startTime)

### Calculate resulting shapefile prioritisation
result_Bio_WDPA <- PUs %>% 
  dplyr::select(ID, Protected)

for(i in 1:length(list_sol_AreaTarget_Bio_WDPA)) {
  sol <- list_sol_AreaTarget_Bio_WDPA[[i]][[2]] %>% 
    dplyr::select(solution_1, ID) %>% 
    st_drop_geometry() %>% 
    tibble()
  
  result_Bio_WDPA <- result_Bio_WDPA %>%
    left_join(sol, by = "ID")
}

result_Bio_WDPA <- result_Bio_WDPA %>%  
  tibble() %>%
  summarise(rank = rowSums(dplyr::select(., (!ID & !geometry & !Protected))), 
            geometry = geometry, 
            Protected = Protected,
            ID = ID) %>% 
  st_as_sf()

result_Bio_WDPA$rank <- -1*(result_Bio_WDPA$rank - 1) + 100

result_Bio_WDPA <- result_Bio_WDPA %>%
  as_tibble %>% 
  dplyr::select(rank, ID) %>%
  left_join(PUs, by = 'ID')

result_Bio_WDPA <- result_Bio_WDPA %>% 
  mutate(rank = case_when(
    rank == 101 ~ 100,
    TRUE ~ .$rank))

result_Bio_WDPA <- result_Bio_WDPA %>%
  st_as_sf

# Save the resulting shapefile
saveRDS(result_Bio_WDPA, paste0("RDS/result_Bio_WDPA.rds"))

################################################################################
# Targets reached
################################################################################

#Amount of the target reached for the benefits

# Optimisation biodiversity and ecosystem services
list_targets_reached_BioServ <- lapply(list_sol_AreaTarget_BioServ, function(x) {
  sol <- x[[2]] %>% 
    dplyr::select(solution_1) 
  
  targets_reached <- eval_target_coverage_summary(x[[1]], sol)
})

# Optimisation biodiversity 
list_targets_reached_Bio <- lapply(list_sol_AreaTarget_Bio, function(x) {
  sol <- x[[2]] %>% 
    dplyr::select(solution_1) 
  
  targets_reached <- eval_target_coverage_summary(x[[1]], sol)
})

# Optimisation biodiversity and ecosystem services
list_targets_reached_BioServ_WDPA <- lapply(list_sol_AreaTarget_BioServ_WDPA, function(x) {
  sol <- x[[2]] %>% 
    dplyr::select(solution_1) 
  
  targets_reached <- eval_target_coverage_summary(x[[1]], sol)
})

# Optimisation biodiversity 
list_targets_reached_Bio_WDPA <- lapply(list_sol_AreaTarget_Bio_WDPA, function(x) {
  sol <- x[[2]] %>% 
    dplyr::select(solution_1) 
  
  targets_reached <- eval_target_coverage_summary(x[[1]], sol)
})