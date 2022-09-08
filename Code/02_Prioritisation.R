#Author: Dabalà Alvise

#Part of the code is adapted from Jeffrey O. Hanson work (see more at: https://prioritizr.net)

#Produce and run all the prioritisations

#Open all the packages needed
library(tidyverse)
library(sf)
library(prioritizr)
library(units)
library(patchwork)
library(mapview)
library(viridis)
library(ggthemes)
library(rnaturalearth)

############################################
#I set the various layer for the analysis

#Prepare the files

#Source the different function
source("Functions/fPlot_GlobalResults.R") #Open the function that plot the results at global scale
source("Functions/fPlot_PUsValues.R") #Open the function that plot the PUs with the values reported in a column
source("Functions/fPlot_Rank.R") #Open the function that plot the results following the value in the rank

#Set the projection
cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

################################################################################

#Open PUs and large_PUs

PUs <- readRDS("RDS/PUs_Splitted.rds") 
PUs_NotSplitted <- readRDS("RDS/PUs_NotSplitted.rds")
Large_PUs <- readRDS("RDS/Large_PUs_40000.rds")

#Percentage of mangroves protected
Percentage_protected <- PUs %>% 
  as_tibble() %>% #Transform to tibble
  group_by(Protected) %>% #Group by protected and not protected
  summarise(sum(AreaGMWKm)) %>% #Sum the value of mangrove area
  dplyr::select(!Protected) %>% #Select only the not protected
  unlist() #Unlist the results

#Results
Percentage_protected[[2]]/(Percentage_protected[[1]]+Percentage_protected[[2]])*100 #Percentage of mangroves protected

#Exact calculation
PUs %>% 
  as_tibble() %>% #Transform to tibble
  summarise(sum(AreaWDPA)) #Sum area of mangroves covered by IUCN I-IV

18730/136752.8*100 #~0.5% more respect to what was estimated

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
                Tot_Carbon) %>% #Select these columns
  summarise(across(everything(), ~ sum(.))) #Sum the values in the columns

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
  dplyr::select(3:67) #Select only the species columns

species <- species %>% 
  summarise_all(sum) #Calculate the range of each species

saveRDS(species, "RDS/species.rds") #Save the RDS

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
#so let's apply the cap these targets following Butchart et al. (2015) → do not change anything in this case
spp_target_percentage_butchart <- ifelse(
  spp_range_size_km2 >= 10000000,
  (1000000 / spp_range_size_km2) * 100,
  spp_target_percentage_rodrigues)

# Select the target for each feature
targets_species <- lapply(species, function(x) { #lapply to all the columns in the species tibble
  a <- spp_range_size_km2 %>% 
    as_tibble() %>% 
    mutate(ID = rownames(.)) %>% 
    slice(which.min(abs(value - x))) %>% #Select the value immediately smaller than the exact range of distribution
    dplyr::select(ID) %>%
    as.numeric()
  
  spp_target_percentage_butchart[[a[[1]]]] #Select the target for that species range of distribution
}
) %>%  
  unname() %>% #Remove the name of each element of the list 
  unlist() #Transform to vector from list

targets_species <- tibble(names = as.character(names(species)), #Column names with the names of all the species
                          amount = as.numeric(targets_species)/100, w = 1) #Column of the target to reach for each species

#Select conservation features columns
ConsFeatures <- PUs %>% 
  st_drop_geometry() %>% 
  dplyr::select(contains(c(names(species))), #Select all the columns that contains the name of the species
                "Tot_Carbon_prc", "Fishing_Intensity_prc", "TOT_STOCK_prc", "POP_prc") %>% #Select the columns with the ecosystem services values scaled 
  summarise(names = as.character(colnames(.)), amount = 0.3) #Produce a column with all the column names and a column with amount = 0.3

ConsFeatures_NotSplitted <- PUs_NotSplitted %>% 
  st_drop_geometry() %>% 
  dplyr::select(contains(c(names(species)))) %>% #Select all the columns that contains the name of the species  
  summarise(names = as.character(colnames(.)), amount = 0.3) #Produce a column with all the column names and a column with amount = 0.3

#Include the weights
ConsFeatures <- ConsFeatures %>% 
  mutate(w = 1) %>% #create a column weights with all the weights = 1
  mutate(amount = ifelse(names %in% c("Tot_Carbon_prc", "Fishing_Intensity_prc", "TOT_STOCK_prc", "POP_prc"), 1, 0.3), #Trasform all the targets for the ecosystem services to 1
         w = ifelse(names %in% c("Tot_Carbon_prc", "Fishing_Intensity_prc", "TOT_STOCK_prc", "POP_prc"), 1, 1)) #Trasform all the ecosystem services weights to 1

for(i in 1:length(species)) {
  #Add species targets
  
  ConsFeatures <- ConsFeatures %>% 
    mutate(amount = case_when( 
      str_detect(ConsFeatures$names, targets_species$names[i]) == TRUE ~ targets_species$amount[i], #When the string in ConsFeatures is the same as the string in targets_species the amount in ConsFeatures is replaced by the amount in targets 
                   TRUE ~ amount))
  
  ConsFeatures_NotSplitted <- ConsFeatures_NotSplitted %>% 
    mutate(amount = case_when(
      str_detect(ConsFeatures_NotSplitted$names, targets_species$names[i]) == TRUE ~ targets_species$amount[i], #When the string in ConsFeatures NotSplitted is the same as the string in targets_species the amount in ConsFeatures is replaced by the amount in targets 
      TRUE ~ amount))
      
}


#Define the type
ConsFeatures <- ConsFeatures %>%
  mutate(type = case_when(amount < 1 ~ "Species", #When the target is <1 the type is species
                          TRUE ~ "EcoServices")) #Otherwise is EcoServices

saveRDS(ConsFeatures, "RDS/ConsFeatures.rds") #Save RDS
saveRDS(ConsFeatures_NotSplitted, "RDS/ConsFeatures_NotSplitted.rds")

################################################################################
# Optimisation biodiversity and ecosystem services
################################################################################

list_sol_AreaTarget_BioServ <- list() #Create a list

startTime <- Sys.time() #Start the time count

PUs$LockedIn <- FALSE #No PUs are LockedIn

for(x in 1:100) {
  
  if(x == 1) { #If x = 1
    p_AreaTarget <- problem(PUs, features = ConsFeatures$names, cost_column = "AreaGMWKm") %>% #Area Target
      add_min_shortfall_objective(sum(PUs$AreaGMWKm*(x/100))) %>% #Maximum budget is x% of the total area
      add_relative_targets(ConsFeatures$amount) %>% # representation targets (Area)
      add_binary_decisions() %>%
      add_gurobi_solver(gap = 1e-4, threads = 8) %>% #Insert the number of threads of your laptop
      #add_rsymphony_solver(verbose = FALSE) %>% 
      add_feature_weights(ConsFeatures$w) #Add the weights
  }
  
  if(x != 1) { #If x > 1
    PUs$LockedIn <- as.logical(sol_AreaTarget$solution_1) #Lock-in all the planning units already selected
    
    p_AreaTarget <- problem(PUs, features = ConsFeatures$names, cost_column = "AreaGMWKm") %>% #Area Target
      add_min_shortfall_objective(sum(PUs$AreaGMWKm*(x/100))) %>% #Maximum budget is x% of the total area
      add_relative_targets(ConsFeatures$amount) %>% # representation targets (Area)
      add_locked_in_constraints(locked_in = "LockedIn") %>% #Planning units are already locked in
      add_binary_decisions() %>%
      add_gurobi_solver(gap = 1e-4, threads = 8) %>% #Insert the number of threads of your laptop
      #add_rsymphony_solver(verbose = FALSE) %>% 
      add_feature_weights(ConsFeatures$w) #Add feature weights
  }
  
  sol_AreaTarget <- solve(p_AreaTarget) #Solve the conservation planning problem
  
  list_sol_AreaTarget_BioServ[[x]] <- list(p_AreaTarget, sol_AreaTarget) #Add the solution to the list of solutions
}

endTime <- Sys.time() #Stop the time count

# prints recorded time
print(endTime - startTime)

### Calculate resulting shapefile
result_BioServ <- PUs %>% 
  dplyr::select(ID, Protected) #Select only the ID and protected areas column

for(i in 1:length(list_sol_AreaTarget_BioServ)) { #Iterate for all the lenght of the list
  sol <- list_sol_AreaTarget_BioServ[[i]][[2]] %>% #Calculate the number of targets reached
    dplyr::select(solution_1, ID) %>% #Select the solution and ID columns
    st_drop_geometry() %>% 
    tibble()
  
  result_BioServ <- result_BioServ %>%
    left_join(sol, by = "ID") #Left join the solution column
}

result_BioServ <- result_BioServ %>%  
  tibble() %>%
  summarise(rank = rowSums(dplyr::select(., (!ID & !geometry & !Protected))), #sum the values of rows that are not ID, geometry and Protected
            geometry = geometry, 
            Protected = Protected,
            ID = ID) %>% 
  st_as_sf() #Transform to shapefile

result_BioServ$rank <- -1*(result_BioServ$rank - 1) + 100 #Invert the rank

result_BioServ <- result_BioServ %>%
  as_tibble %>% #Transform to tibble
  dplyr::select(rank, ID) %>% #Select only rank and ID
  left_join(PUs, by = 'ID')

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

list_sol_AreaTarget_BioServ_WDPA <- list() #List

startTime <- Sys.time() #Start the time count

PUs$LockedIn <- PUs$Protected #Pus Locked-in are all the PUs already protected

for(x in 14:100) {
  
  if(x != 14) {PUs$LockedIn <- as.logical(sol_AreaTarget$solution_1)} else #PUs Locked-In are all the PUs selected in the solution when x != 14
  {PUs$LockedIn <- as.logical(PUs$Protected)} #If x = 14 the LockedIn planning units are those already selected
  
  p_AreaTarget <- problem(PUs, features = ConsFeatures$names, cost_column = "AreaGMWKm") %>% #Area Target
    add_min_shortfall_objective(sum(PUs$AreaGMWKm*(x/100))) %>% #Maximum cost is x% of the total area
    add_relative_targets(ConsFeatures$amount) %>% # representation targets (Area)
    add_locked_in_constraints(locked_in = "LockedIn") %>% #LockedIn areas
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 1e-4, threads = 8) %>% #Insert your number of threads
    #add_rsymphony_solver(verbose = FALSE) %>% 
    add_feature_weights(ConsFeatures$w) #Add weights
  
  sol_AreaTarget <- solve(p_AreaTarget) #Solve the conservation planning problem
  
  list_sol_AreaTarget_BioServ_WDPA[[x-13]] <- list(p_AreaTarget, sol_AreaTarget) #List of solution
}

endTime <- Sys.time() #End time count

# prints recorded time
print(endTime - startTime)

### Calculate resulting shapefile
result_BioServ_WDPA <- PUs %>% 
  dplyr::select(ID, Protected) #Select only ID and protected

for(i in 1:length(list_sol_AreaTarget_BioServ_WDPA)) {
  sol <- list_sol_AreaTarget_BioServ_WDPA[[i]][[2]] %>% #Select solutions
    dplyr::select(solution_1, ID) %>% #Select columns solution and ID
    st_drop_geometry() %>% #Drop geometry
    tibble()
  
  result_BioServ_WDPA <- result_BioServ_WDPA %>%
    left_join(sol, by = "ID") #Left join the solution column to the results shapefile
}

result_BioServ_WDPA <- result_BioServ_WDPA %>%  
  tibble() %>%
  summarise(rank = rowSums(dplyr::select(., (!ID & !geometry & !Protected))), #Sum rowise of the values of all the columns that are not ID, geometry or Protected
            geometry = geometry, 
            Protected = Protected,
            ID = ID) %>% 
  st_as_sf() #Transform to sf

result_BioServ_WDPA$rank <- -1*(result_BioServ_WDPA$rank - 1) + 100 #Invert the rank (lower rank = selected earlier)

# Priority areas by country
result_BioServ_WDPA <- result_BioServ_WDPA %>%
  as_tibble %>% #Transform to tibble
  dplyr::select(rank, ID) %>% #Select only rank and ID
  left_join(PUs, by = 'ID')

result_BioServ_WDPA_rmPA <- result_BioServ_WDPA %>% 
  filter(Protected == FALSE) %>% #Remove all the areas that are already protected
  st_as_sf

result_BioServ_WDPA <- result_BioServ_WDPA %>%
  st_as_sf

# Save the resulting shapefile
saveRDS(result_BioServ_WDPA, "RDS/result_BioServ_WDPA.rds")
saveRDS(result_BioServ_WDPA_rmPA, "RDS/result_BioServ_WDPA_rmPA.rds")

plot_global_map <- fPlot_Rank(result_BioServ_WDPA_rmPA, Large_PUs, palet = "plasma",
                              brk = c(14, 25, 50, 75, 100), lm = c(14, 100)) 
ggsave(plot = plot_global_map, paste0("Figures/Rank_Global_40000_WDPA.pdf"),
       dpi = 1000, width = 18, height = 9, units = "cm", limitsize = FALSE)

plot_results <- fPlot_PUsValues(result_BioServ_WDPA_rmPA, "rank", scale_fill = "plasma") 
ggsave(plot = plot_results, paste0("Figures/Rank_Global_WDPA.svg"),
       dpi = 1000, width = 18, height = 9, units = "cm", limitsize = FALSE)

################################################################################
# Optimisation biodiversity
################################################################################

# Select conservation features
ConsFeatures <- ConsFeatures %>% 
  dplyr::filter(type != "EcoServices") #Remove EcoServices from the conservation features

list_sol_AreaTarget_Bio <- list() #Create list for the solutions

startTime <- Sys.time() #Start time count

PUs$LockedIn <- FALSE #PUs are not LockedIn

for(x in 1:100) {
  
  if(x == 1) {
    p_AreaTarget_Bio <- problem(PUs, features = ConsFeatures$names, cost_column = "AreaGMWKm") %>% #Area Target
      add_min_shortfall_objective(sum(PUs$AreaGMWKm*(x/100))) %>% #Maximum cost is x% of the total area
      add_relative_targets(ConsFeatures$amount) %>% #representation targets (Area)
      add_binary_decisions() %>%
      add_gurobi_solver(gap = 1e-4, threads = 8) %>% #Insert your number of threads
      #add_rsymphony_solver(verbose = FALSE) %>% 
      add_feature_weights(ConsFeatures$w)
  }
  
  if(x != 1) {
    PUs$LockedIn <- as.logical(sol_AreaTarget_Bio$solution_1) #LockedIn areas are those already selected
    
    p_AreaTarget_Bio <- problem(PUs, features = ConsFeatures$names, cost_column = "AreaGMWKm") %>% #Area Target
      add_min_shortfall_objective(sum(PUs$AreaGMWKm*(x/100))) %>% #Maximum cost is 30% of the total area
      add_relative_targets(ConsFeatures$amount) %>% # representation targets (Area)
      add_locked_in_constraints(locked_in = "LockedIn") %>% 
      add_binary_decisions() %>%
      add_gurobi_solver(gap = 1e-4, threads = 8) %>% 
      #add_rsymphony_solver(verbose = FALSE) %>% 
      add_feature_weights(ConsFeatures$w)
  }
  
  sol_AreaTarget_Bio <- solve(p_AreaTarget_Bio) #Solution conservation problem
  
  list_sol_AreaTarget_Bio[[x]] <- list(p_AreaTarget_Bio, sol_AreaTarget_Bio) #Add solution to the list
}

endTime <- Sys.time() #End time count

# prints recorded time
print(endTime - startTime)

### Calculate resulting shapefile prioritisation
result_Bio <- PUs %>% 
  dplyr::select(ID, Protected) #Select only ID and protected

for(i in 1:length(list_sol_AreaTarget_Bio)) {
  sol <- list_sol_AreaTarget_Bio[[i]][[2]] %>% #Select solutions
    dplyr::select(solution_1, ID) %>% 
    st_drop_geometry() %>% 
    tibble()
  
  result_Bio <- result_Bio %>%
    left_join(sol, by = "ID") #Left join column of solutions
}

result_Bio <- result_Bio %>%  
  tibble() %>%
  summarise(rank = rowSums(dplyr::select(., (!ID & !geometry & !Protected))), #Rowise sum by column of the solutions
            geometry = geometry, 
            Protected = Protected,
            ID = ID) %>% 
  st_as_sf()

result_Bio$rank <- -1*(result_Bio$rank - 1) + 100 #Invert the rank

result_Bio <- result_Bio %>%
  as_tibble %>% 
  dplyr::select(rank, ID) %>% #Select only rank and ID
  left_join(PUs, by = 'ID')

result_Bio <- result_Bio %>% 
  mutate(rank = case_when(
    rank == 101 ~ 100, #I transform all the rank 101 (those PUs that are never selected 
    #cause the prioritisation do not reach the end, cause no conservation features have target 100%), to rank 100
    TRUE ~ .$rank)) %>% 
  st_as_sf()

# Save the resulting shapefile
saveRDS(result_Bio, "RDS/result_Bio.rds")
# 
# plot_global_map <- fPlot_Rank(result_Bio, Large_PUs, palet = "viridis") 
# ggsave(plot = plot_global_map, "Figures/Rank_Global_Bio_40000.svg",
#        dpi = 1000, width = 18, height = 9, units = "cm", limitsize = FALSE)
# 
# plot_results <- fPlot_PUsValues(result_Bio, "rank", scale_fill = "viridis") 
# ggsave(plot = plot_results, "Figures/Rank_Global_Bio.svg",
#        dpi = 1000, width = 18, height = 9, units = "cm", limitsize = FALSE)

################################################################################
# Optimisation biodiversity building on already protected areas
################################################################################

list_sol_AreaTarget_Bio_WDPA <- list()

startTime <- Sys.time()

PUs$LockedIn <- PUs$Protected

for(x in 14:100) {
  
  if(x != 14) {PUs$LockedIn <- as.logical(sol_AreaTarget$solution_1)} else
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
  
  list_sol_AreaTarget_Bio_WDPA[[x-13]] <- list(p_AreaTarget, sol_AreaTarget)
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