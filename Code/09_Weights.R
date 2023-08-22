#Author: Dabalà Alvise

#Part of the code is adapted from Jeffrey O. Hanson work (see more at: https://prioritizr.net)

#Produce and run all the prioritisations

#Open all the packages needed
pacman::p_load(tidyverse, sf, prioritizr, units, patchwork, mapview, viridis, ggthemes, rnaturalearth)

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

PUs <- readRDS("RDS/PUs_Splitted_I_IV_and_All_9111.rds") 
PUs_NotSplitted <- readRDS("RDS/PUs_NotSplitted.rds")
Large_PUs <- readRDS("RDS/Large_PUs.rds")

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

19403/136752.8*100 #~0.5% more respect to what was estimated

#Scaling
library(scales)
library(openxlsx)

#I transform all the values to numeric
PUs <- PUs %>% 
  mutate(across(!geometry & !Protected & !Protected_I_VI & !country & !continent, 
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
  dplyr::select(!AreaGMWKm) %>% 
  dplyr::select(3:67) #Select only the species columns

species <- species %>% 
  summarise_all(sum) #Calculate the range of each species

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

for(l in c(10, 100, 1000)) {
  #Include the weights
  ConsFeatures <- ConsFeatures %>% 
    mutate(w = 1) %>% #create a column weights with all the weights = 1
    mutate(amount = ifelse(names %in% c("Tot_Carbon_prc", "Fishing_Intensity_prc", "TOT_STOCK_prc", "POP_prc"), 1, 0.3), #Trasform all the targets for the ecosystem services to 1
           w = ifelse(names %in% c("Tot_Carbon_prc", "Fishing_Intensity_prc", "TOT_STOCK_prc", "POP_prc"), l, 1)) #Trasform all the ecosystem services weights to 10
  
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
    
    sol_AreaTarget <- solve(p_AreaTarget, force = TRUE) #Solve the conservation planning problem
    
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
    left_join(PUs, by = 'ID') %>% 
    st_as_sf() #Transform to shapefile
  
  # Save the resulting shapefile
  saveRDS(result_BioServ, paste0("RDS/1e-4/gurobi/result_BioServ_", i,".rds"))
}

################################################################################
result_BioServ_10 <- readRDS("RDS/1e-4/gurobi/result_BioServ_10.rds")
result_BioServ_100 <- readRDS("RDS/1e-4/gurobi/result_BioServ_100.rds")
result_BioServ_1000 <- readRDS("RDS/1e-4/gurobi/result_BioServ_1000.rds")
result_Bio <- readRDS("RDS/1e-4/gurobi/result_Bio.rds")

#Calculate number of biodiversity features that reach the target increasing the area target

# Biodiversity and ecosystem services

# Calculate when each species reach the targets

targets_reached_final <- list() #List of the targets reached

targets_reached <- lapply(list(result_BioServ_10$rank, result_BioServ_100$rank,
                               result_BioServ_1000$rank, result_Bio$rank
),
function(x) {
  for(i in min(x):(max(x)-1)) { 
    targets_reached <- PUs %>% 
      dplyr::select(contains(c(names(species)))) %>% #Select only species col
      bind_cols(rank = x) %>% #Add rank column
      st_drop_geometry() %>% #drop geometry
      as_tibble() %>% #Transform to tibble
      group_by(selected = rank <= i) %>% #Group <= i or >= i
      summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop') %>% #sum each column value by group
      dplyr::select(!rank) %>% #Select all the columns that are not rank
      pivot_longer(!selected, names_to = "names", values_to = "Area") %>% #Transform in long format, names is the name of the columns, area is the area selected or not selected
      pivot_wider(names_from = selected, values_from = "Area") %>% #Trasform to long format the area selected or not selected
      left_join(ConsFeatures, by = "names") #Add the minimum target to reach
    
    targets_reached <- targets_reached %>%
      #mutate(names = gsub("_.*","", targets_reached$names)) %>%
      mutate(amount_protected = `TRUE`/(`FALSE` + `TRUE`)) %>% #Calculate the percentage protected
      mutate(protected = case_when(amount_protected < amount ~ 0, #It is not protected when the percentage protected is < than the target
                                   TRUE ~ 1)) #%>% 
    #mutate(shortfall = amount - amount_protected)
    #group_by(names) %>%
    #summarise(protected = sum(protected)/n()) %>%
    #mutate(protected = case_when(protected < 1 ~ 0,
    #                             TRUE ~ 1))
    
    targets_reached_final[[i]] <- targets_reached
  }
  targets_reached_final
}
)

# Calculate the number of species that reach the targets for incremental rankings
ntarget_reached_df_BioServ_10 <- tibble(prct = c(0, 100), reached = c(0, 100)) #Add the number of targets reached for 0% and 100%

for(i in min(result_BioServ_10$rank):(max(result_BioServ_10$rank)-1)) {
  
  ntarget_reached_df_BioServ_10 <- ntarget_reached_df_BioServ_10 %>% 
    add_row(prct = i, reached = sum(targets_reached[[1]][[i]]$protected)/944*100) # Add a row that calculate percentage of target reached on the total of 944 for each incremental area target i
}

ntarget_reached_df_BioServ_100 <- tibble(prct = c(0, 100), reached = c(0, 100)) #Add the number of targets reached for 0% and 100%

for(i in min(result_BioServ_100$rank):(max(result_BioServ_100$rank)-1)) {
  
  ntarget_reached_df_BioServ_100 <- ntarget_reached_df_BioServ_100 %>% 
    add_row(prct = i, reached = sum(targets_reached[[2]][[i]]$protected)/944*100) # Add a row that calculate percentage of target reached on the total of 944 for each incremental area target i
}

ntarget_reached_df_BioServ_1000 <- tibble(prct = c(0, 100), reached = c(0, 100)) #Add the number of targets reached for 0% and 100%

for(i in min(result_BioServ_1000$rank):(max(result_BioServ_1000$rank)-1)) {
  
  ntarget_reached_df_BioServ_1000 <- ntarget_reached_df_BioServ_1000 %>% 
    add_row(prct = i, reached = sum(targets_reached[[3]][[i]]$protected)/944*100) # Add a row that calculate percentage of target reached on the total of 944 for each incremental area target i
}

ntarget_reached_df_Bio <- tibble(prct = c(0, 100), reached = c(0, 100)) #Add rows with the percentage of targets reached for 0 and 100 area budget

for(i in min(result_Bio$rank):(max(result_Bio$rank)-1)) {
  
  ntarget_reached_df_Bio <- ntarget_reached_df_Bio %>% 
    add_row(prct = i, reached = sum(targets_reached[[4]][[i]]$protected)/944*100) #Add rows with the percentage of targets reached for each area budget
}

################################################################################
#Plot biodiversity features targets reached

fplot_targets <- function(ntarget_reached) {
  ggplot(data = ntarget_reached, aes(x = prct, y = reached, colour = factor(method, 
                                                                            levels = c("Biodiversity", "Biodiversity & ecosystem services")))) +
    geom_point(size = 0.5) +
    geom_line(size = 0.5) +
    scale_color_manual(name= "Targets reached",
                       labels=c(c("Biodiversity",
                                  "Biodiversity & ecosystem services")),
                       values=c("#2D3047",
                                "#93B7BE")) +
    xlab("Mangroves selected in priority areas (%)") +
    ylab("Biodiversity targets reached (%)") +
    theme_bw(base_size = 6.5) +
    theme(legend.position = "none",
          legend.title = element_blank()#,
          #legend.background = element_rect(fill="NA", size=0.5, linetype="solid", colour ="NA")
    ) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 101)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 101))
}

#Plot increase features reached targets
ntarget_reached_df_BioServ_10 <- ntarget_reached_df_BioServ_10 %>% 
  mutate(method = "Biodiversity & ecosystem services") 

ntarget_reached_df_BioServ_100 <- ntarget_reached_df_BioServ_100 %>% 
  mutate(method = "Biodiversity & ecosystem services") 

ntarget_reached_df_BioServ_1000 <- ntarget_reached_df_BioServ_1000 %>% 
  mutate(method = "Biodiversity & ecosystem services") 

ntarget_reached_df_Bio <- ntarget_reached_df_Bio %>% 
  mutate(method = "Biodiversity")

ntarget_reached_comparison_10 <- ntarget_reached_df_BioServ_10 %>% 
  rbind(ntarget_reached_df_Bio)

ntarget_reached_comparison_100 <- ntarget_reached_df_BioServ_100 %>% 
  rbind(ntarget_reached_df_Bio)

ntarget_reached_comparison_1000 <- ntarget_reached_df_BioServ_1000 %>% 
  rbind(ntarget_reached_df_Bio)

#Plot 10
plot_TargetsReached_10 <- fplot_targets(ntarget_reached_comparison_10) +
  geom_vline(xintercept = 13.5, colour = "black", linewidth = 0.3, linetype = "dashed")

#Plot 100
plot_TargetsReached_100 <- fplot_targets(ntarget_reached_comparison_100) +
  geom_vline(xintercept = 13.5, colour = "black", linewidth = 0.3, linetype = "dashed") #Add a dashed line at 13.5% that is the percentage of mangroves already protected

#Plot 1000
plot_TargetsReached_1000 <- fplot_targets(ntarget_reached_comparison_1000) +
  geom_vline(xintercept = 13.5, colour = "black", size = 0.3, linetype = "dashed") #Add a dashed line at 13.5% that is the percentage of mangroves already protected

################################################################################
#Increase benefits for increasing targets
Total_Services <- PUs %>% 
  as_tibble %>% 
  summarise(Fishing = sum(Fishing_Intensity*AreaGMWKm), #sum of the total fishing intensity
            People = sum(POP*AreaGMWKm), #sum of the total number of people
            Properties = sum(TOT_STOCK*AreaGMWKm), #total number of properties
            Carbon = sum(Tot_Carbon*AreaGMWKm), #total carbon
            AreaGMWKm = sum(AreaGMWKm)) #total area

PA_Services <- PUs %>% 
  as_tibble %>% 
  filter(Protected == TRUE) %>% #select only planning units already protected
  summarise(Fishing = sum(Fishing_Intensity*AreaGMWKm), 
            People = sum(POP*AreaGMWKm),
            Properties = sum(TOT_STOCK*AreaGMWKm),
            Carbon = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm = sum(AreaGMWKm))

Increase_EcoServices_10 <- c() #create a list

for (i in 1:max(result_BioServ_10$rank)) {
  EcoServ <- result_BioServ_10 %>% 
    filter(rank <= i) %>% #keep only the planning units with rank <= i
    as_tibble %>% 
    summarise(Fishing = sum(Fishing_Intensity*AreaGMWKm), 
              People = sum(POP*AreaGMWKm),
              Properties = sum(TOT_STOCK*AreaGMWKm),
              Carbon = sum(Tot_Carbon*AreaGMWKm),
              AreaGMWKm = sum(AreaGMWKm))
  
  Increase_EcoServices_10 <- Increase_EcoServices_10 %>% 
    rbind(EcoServ) #bind rows
}

rm(EcoServ)

Increase_EcoServices_10 <- Increase_EcoServices_10 %>% 
  mutate(prct = as.numeric(rownames(.))) #prct is the name of the rows

Increase_EcoServices_100 <- c()

for (i in 1:max(result_BioServ_100$rank)) {
  EcoServ <- result_BioServ_100 %>% 
    filter(rank <= i) %>% #keep all the rows of rank <= i
    as_tibble %>% 
    summarise(Fishing = sum(Fishing_Intensity*AreaGMWKm), 
              People = sum(POP*AreaGMWKm),
              Properties = sum(TOT_STOCK*AreaGMWKm),
              Carbon = sum(Tot_Carbon*AreaGMWKm),
              AreaGMWKm = sum(AreaGMWKm))
  
  Increase_EcoServices_100 <- Increase_EcoServices_100 %>% 
    rbind(EcoServ)
}

rm(EcoServ)

Increase_EcoServices_100 <- Increase_EcoServices_100 %>% 
  mutate(prct = as.numeric(rownames(.)))

Increase_EcoServices_1000 <- c()

for (i in 1:max(result_BioServ_1000$rank)) {
  EcoServ <- result_BioServ_1000 %>% 
    filter(rank <= i) %>% #keep all the rows of rank <= i
    as_tibble %>% 
    summarise(Fishing = sum(Fishing_Intensity*AreaGMWKm), 
              People = sum(POP*AreaGMWKm),
              Properties = sum(TOT_STOCK*AreaGMWKm),
              Carbon = sum(Tot_Carbon*AreaGMWKm),
              AreaGMWKm = sum(AreaGMWKm))
  
  Increase_EcoServices_1000 <- Increase_EcoServices_1000 %>% 
    rbind(EcoServ)
}

rm(EcoServ)

Increase_EcoServices_1000 <- Increase_EcoServices_1000 %>% 
  mutate(prct = as.numeric(rownames(.)))

#Calculate the percentage
Increase_EcoServices_10_Prct <- Increase_EcoServices_10 %>% 
  summarise(Fishing = Fishing/Total_Services$Fishing, #Calculate the percentage of services
            People = People/Total_Services$People,
            Properties = Properties/Total_Services$Properties,
            Carbon = Carbon/Total_Services$Carbon,
            prct = prct)

Increase_EcoServices_100_Prct <- Increase_EcoServices_100 %>% 
  summarise(Fishing = Fishing/Total_Services$Fishing, #Calculate the percentage of services
            People = People/Total_Services$People,
            Properties = Properties/Total_Services$Properties,
            Carbon = Carbon/Total_Services$Carbon,
            prct = prct)

Increase_EcoServices_1000_Prct <- Increase_EcoServices_1000 %>% 
  summarise(Fishing = Fishing/Total_Services$Fishing, #Calculate the percentage of services
            People = People/Total_Services$People,
            Properties = Properties/Total_Services$Properties,
            Carbon = Carbon/Total_Services$Carbon,
            prct = prct)

PA_Services <- PA_Services %>% 
  mutate(prct = 13.5) #Select the percentage of area protected by protected areas

#Percentage of each biodiversity feature protected for increasing targets
fPlot_EcoServ_Increase <- function(Increase, PA_Services) {
  
  if(nrow(Increase) < 100) {
    #Transform to long format
    long_Increase <- Increase %>% 
      pivot_longer(!prct, names_to = "benefit", values_to = "value")
  } else {
    #Transform to long format
    long_Increase <- Increase %>% 
      rbind(tibble(Fishing = 0, People = 0, Properties = 0, Carbon = 0, prct = 0)) %>% 
      pivot_longer(!prct, names_to = "benefit", values_to = "value")
  }
  
  #Plot of the increase of the benefit protected for increasing targets
  ggplot(data = long_Increase, aes(x = prct, y = value*100, colour = factor(benefit, levels = c("Fishing", "People", "Properties", "Carbon")))) +
    geom_line(size = 0.5) +
    geom_point(size = 0.5) +
    scale_color_manual(name = "Features",
                       labels = c(c("Fishing",
                                    "People", 
                                    "Properties",
                                    "Carbon")),
                       values = c("#1E88E5","#FFC107","#D81B60", "#004D40")) +
    xlab("Mangroves selected in priority areas (%)") +
    ylab("Service protected (%)") +
    theme_bw(base_size = 6.5) +
    theme(legend.position = "none",
          legend.title = element_blank()#,
          #legend.background = element_rect(fill="NA", size=0.5, linetype="solid", colour ="NA")
    ) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 101)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 101)) +
    geom_vline(xintercept = PA_Services$prct, colour = "black", size=0.3, linetype = "dashed")
}

# Ecosystem services and biodiversity

Plot_Increase_10 <- fPlot_EcoServ_Increase(Increase_EcoServices_10_Prct, PA_Services)
Plot_Increase_100 <- fPlot_EcoServ_Increase(Increase_EcoServices_100_Prct, PA_Services)
Plot_Increase_1000 <- fPlot_EcoServ_Increase(Increase_EcoServices_1000_Prct, PA_Services)

p1 <- plot_TargetsReached_10 + 
  theme(plot.tag = element_text(face = 'bold',  size = 10)) +
  Plot_Increase_10 +
  theme(plot.tag = element_text(face = 'bold',  size = 10)) +
  plot_annotation(tag_levels = list(c('a)','b)')),
                  title = 'Weight ecosystem services = 10',
                  theme = theme(plot.title = element_text(face = 'bold', size = 10))) +
  plot_layout(ncol = 2)

p2 <- plot_TargetsReached_100 + 
  theme(plot.tag = element_text(face = 'bold',  size = 10)) +
  Plot_Increase_100 +
  theme(plot.tag = element_text(face = 'bold',  size = 10)) +
  plot_annotation(tag_levels = list(c('c)','d)')),
                  title = 'Weight ecosystem services = 100',
                  theme = theme(plot.title = element_text(face = 'bold', size = 10))) +
  plot_layout(ncol = 2)

p3 <- plot_TargetsReached_1000 +
  theme(plot.tag = element_text(face = 'bold',  size = 10)) +
  Plot_Increase_1000 +
  theme(plot.tag = element_text(face = 'bold',  size = 10)) +
  plot_annotation(tag_levels = list(c('e)','f)')),
                  title = 'Weight ecosystem services = 1000',
                  theme = theme(plot.title = element_text(face = 'bold', size = 10))) +
  plot_layout(ncol = 2)

ptot <- wrap_elements(p1) / wrap_elements(p2) / wrap_elements(p3)

ggsave("Figures/gurobi/Targets_IncreaseServices_Weights.svg",
       dpi = 1000, units = "cm", width = 16, height = 21) 

ntarget_reached_df_Bio <- ntarget_reached_df_Bio %>% 
  rename(reached_Biodiversity = reached) %>% 
  dplyr::select(!method)

ntarget_reached_df_BioServ_10 <- ntarget_reached_df_BioServ_10 %>% 
  rename(reached_Biodiversity_ES = reached) %>% 
  dplyr::select(!method)

SuppFig10A <- ntarget_reached_df_Bio %>% 
  left_join(ntarget_reached_df_BioServ_10, by = "prct")

ntarget_reached_df_BioServ_100 <- ntarget_reached_df_BioServ_100 %>% 
  rename(reached_Biodiversity_ES = reached) %>% 
  dplyr::select(!method)

SuppFig10C <- ntarget_reached_df_Bio %>% 
  left_join(ntarget_reached_df_BioServ_100, by = "prct")

ntarget_reached_df_BioServ_1000 <- ntarget_reached_df_BioServ_1000 %>% 
  rename(reached_Biodiversity_ES = reached) %>% 
  dplyr::select(!method)

SuppFig10E <- ntarget_reached_df_Bio %>% 
  left_join(ntarget_reached_df_BioServ_1000, by = "prct")

#Data Source Supplementary Fig.10
SuppFig10 <- list(SuppFig10A, Increase_EcoServices_10_Prct, 
                  SuppFig10C, Increase_EcoServices_100_Prct, 
                  SuppFig10E, Increase_EcoServices_1000_Prct)

names(SuppFig10) <- c("SuppFig10A", "SuppFig10B", "SuppFig10C", "SuppFig10D", "SuppFig10E", "SuppFig10F")

saveRDS(SuppFig10, "RDS/SuppFig10.rds")
