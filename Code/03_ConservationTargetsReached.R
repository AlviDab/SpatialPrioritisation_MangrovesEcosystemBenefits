#Author: Dabalà Alvise

#Analysis of the number of conservation targets reached for incremental area budgets
#Calculation and plot of the amount of ecosystem services protected

library(tidyverse)
library(sf)
library(knitr)
library(patchwork)
library(viridis)
library(ggthemes)

## Biodiversity and ecosystem services

# Read the results
result_BioServ <- readRDS("RDS/result_BioServ.rds")
result_BioServ_WDPA <- readRDS("RDS/result_BioServ_WDPA.rds")
result_Bio <- readRDS("RDS/result_Bio.rds")
result_Bio_WDPA <- readRDS("RDS/result_Bio_WDPA.rds")
PUs <- readRDS("RDS/PUs_Splitted.rds")
PUs_NotSplitted <- readRDS("RDS/PUs_NotSplitted.rds")
ConsFeatures <- readRDS("RDS/ConsFeatures.rds")
ConsFeatures_NotSplitted <- readRDS("RDS/ConsFeatures_NotSplitted.rds")
species <- readRDS("RDS/species.rds")

#Calculate number of biodiversity features that reach the target increasing the area target

# Biodiversity and ecosystem services

# Calculate when each species reach the targets

targets_reached_final <- list() #List of the targets reached

targets_reached <- lapply(list(result_BioServ$rank, result_BioServ_WDPA$rank,
                               result_Bio$rank, result_Bio_WDPA$rank 
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

targets_reached_PAs <- PUs %>%
    dplyr::select(contains(c(names(species))), Protected) %>% #Select only species col and protected column
    st_drop_geometry() %>% 
    as_tibble() %>% 
    group_by(Protected) %>% #Group if protected or not
    summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop') %>% #sum each column value by group
    pivot_longer(!Protected, names_to = "names", values_to = "Area") %>% #Transform to long format, names are the name of each column, values are from the amount of area protected or not
    pivot_wider(names_from = Protected, values_from = "Area") %>% #Transform to wide format the area column from values of protected or not protected 
    left_join(ConsFeatures, by = "names") #Add the minimum amount of service 
  
targets_reached_PAs <- targets_reached_PAs %>% 
  #mutate(names = gsub("_.*","", targets_reached_PAs$names)) %>% 
  mutate(amount_protected = `TRUE`/(`FALSE` + `TRUE`)) %>% #Percentage of protected and not protected area by conservation feature
  mutate(protected = case_when(amount_protected < amount ~ 0, #When amount protected < target the result is 0
                               TRUE ~ 1)) %>% 
  #group_by(names) %>% 
  #summarise(protected = sum(protected)/n()) %>% 
  filter(protected == TRUE) %>% #Select only planning units with result 1
  nrow() #number of planning units that reach the targets

# Calculate the number of species that reach the targets for incremental rankings
ntarget_reached_df_BioServ <- tibble(prct = c(0, 100), reached = c(0, 100)) #Add the number of targets reached for 0% and 100%

for(i in min(result_BioServ$rank):(max(result_BioServ$rank)-1)) {
  
  ntarget_reached_df_BioServ <- ntarget_reached_df_BioServ %>% 
    add_row(prct = i, reached = sum(targets_reached[[1]][[i]]$protected)/944*100) # Add a row that calculate percentage of target reached on the total of 944 for each incremental area target i
}

ntarget_reached_df_BioServ_WDPA <- tibble(prct = c(13.1, 100), reached = c(targets_reached_PAs/944*100, 100)) #create a tibble with the amounts of target reached by already protected areas and going to 100%

for(i in min(result_BioServ_WDPA$rank):(max(result_BioServ_WDPA$rank)-1)) {
  
  ntarget_reached_df_BioServ_WDPA <- ntarget_reached_df_BioServ_WDPA %>% 
    add_row(prct = i, reached = sum(targets_reached[[2]][[i]]$protected)/944*100) #Add rows with the percentage of targets reached for each area budget
}

ntarget_reached_df_Bio <- tibble(prct = c(0, 100), reached = c(0, 100)) #Add rows with the percentage of targets reached for 0 and 100 area budget

for(i in min(result_Bio$rank):(max(result_Bio$rank)-1)) {
  
  ntarget_reached_df_Bio <- ntarget_reached_df_Bio %>% 
    add_row(prct = i, reached = sum(targets_reached[[3]][[i]]$protected)/944*100) #Add rows with the percentage of targets reached for each area budget
}

ntarget_reached_df_Bio_WDPA <- tibble(prct = c(13.1, 100), reached = c(targets_reached_PAs/944*100, 100)) #Create tibble with rows that report the percentage of targets reached for already protected areas and 100 area budget

for(i in min(result_Bio_WDPA$rank):(max(result_Bio_WDPA$rank)-1)) {
  
  ntarget_reached_df_Bio_WDPA <- ntarget_reached_df_Bio_WDPA %>% 
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
ntarget_reached_df_BioServ <- ntarget_reached_df_BioServ %>% 
  mutate(method = "Biodiversity & ecosystem services") 

ntarget_reached_df_Bio <- ntarget_reached_df_Bio%>% 
  mutate(method = "Biodiversity")

ntarget_reached_df_BioServ_WDPA <- ntarget_reached_df_BioServ_WDPA %>% 
  mutate(method = "Biodiversity & ecosystem services")

ntarget_reached_df_Bio_WDPA <- ntarget_reached_df_Bio_WDPA %>% 
  mutate(method = "Biodiversity")

ntarget_reached_comparison_noWDPA <- ntarget_reached_df_BioServ %>% 
  rbind(ntarget_reached_df_Bio)

ntarget_reached_comparison_WDPA <- ntarget_reached_df_BioServ_WDPA %>% 
  rbind(ntarget_reached_df_Bio_WDPA)

#Plot NoWDPA
plot_TargetsReached <- fplot_targets(ntarget_reached_comparison_noWDPA) +
  geom_vline(xintercept = 13.1, colour = "black", size = 0.3, linetype = "dashed")

ggsave("Figures/TargetsReachedSpecies_noWDPA.pdf", 
       dpi = 1000, units = "cm", width = 8, height = 6)

#Plot WDPA
#Plot for species

plot_TargetsReached_WDPA <- fplot_targets(ntarget_reached_comparison_WDPA) +
  geom_vline(xintercept = 13.1, colour = "black", size = 0.3, linetype = "dashed") #Add a dashed line at 13.1% that is the percentage of mangroves already protected

ggsave("Figures/TargetsReachedSpecies_WDPA.pdf", 
       dpi = 1000, units = "cm", width = 8, height = 6)

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

Increase_EcoServices <- c() #create a list

for (i in 1:max(result_BioServ$rank)) {
  EcoServ <- result_BioServ %>% 
    filter(rank <= i) %>% #keep only the planning units with rank <= i
    as_tibble %>% 
    summarise(Fishing = sum(Fishing_Intensity*AreaGMWKm), 
              People = sum(POP*AreaGMWKm),
              Properties = sum(TOT_STOCK*AreaGMWKm),
              Carbon = sum(Tot_Carbon*AreaGMWKm),
              AreaGMWKm = sum(AreaGMWKm))
  
  Increase_EcoServices <- Increase_EcoServices %>% 
    rbind(EcoServ) #bind rows
}

rm(EcoServ)

Increase_EcoServices <- Increase_EcoServices %>% 
  mutate(prct = as.numeric(rownames(.))) #prct is the name of the rows

Increase_EcoServices_WDPA <- c()

for (i in 1:max(result_BioServ_WDPA$rank)) {
  EcoServ <- result_BioServ_WDPA %>% 
    filter(rank <= i) %>% #keep all the rows of rank <= i
    as_tibble %>% 
    summarise(Fishing = sum(Fishing_Intensity*AreaGMWKm), 
              People = sum(POP*AreaGMWKm),
              Properties = sum(TOT_STOCK*AreaGMWKm),
              Carbon = sum(Tot_Carbon*AreaGMWKm),
              AreaGMWKm = sum(AreaGMWKm))
  
  Increase_EcoServices_WDPA <- Increase_EcoServices_WDPA %>% 
    rbind(EcoServ)
}

rm(EcoServ)

Increase_EcoServices_WDPA <- Increase_EcoServices_WDPA %>% 
  mutate(prct = as.numeric(rownames(.)))

PA_Services <- PA_Services %>% 
  mutate(prct = 13.1) #Select the percentage of area protected by protected areas

Increase_EcoServices_WDPA <- Increase_EcoServices_WDPA %>% 
  rbind(PA_Services) %>% 
  filter(AreaGMWKm != 0) #Remove all the planning units covered by 0 km² of mangroves 

Increase_EcoServices_Prct <- Increase_EcoServices %>% 
  summarise(Fishing = Fishing/Total_Services$Fishing, #Calculate the percentage of services
            People = People/Total_Services$People,
            Properties = Properties/Total_Services$Properties,
            Carbon = Carbon/Total_Services$Carbon,
            prct = prct)
  
Increase_EcoServices_WDPA_Prct <- Increase_EcoServices_WDPA %>% 
  summarise(Fishing = Fishing/Total_Services$Fishing, #Calculate the percentage of services
            People = People/Total_Services$People,
            Properties = Properties/Total_Services$Properties,
            Carbon = Carbon/Total_Services$Carbon,
            prct = prct)

#Percentage of each biodiversity feature protected for increasing targets
fPlot_EcoServ_Increase <- function(Increase) {
  
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

Plot_Increase <- fPlot_EcoServ_Increase(Increase_EcoServices_Prct)

# Ecosystem services and biodiversity WDPA

Plot_Increase_WDPA <- fPlot_EcoServ_Increase(Increase_EcoServices_WDPA_Prct)

plot_TargetsReached + Plot_Increase +
  plot_TargetsReached_WDPA + Plot_Increase_WDPA +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a') +
  theme(plot.tag = element_text(face = 'bold'))

# plot_TargetsReached_10 + Plot_Increase_10 + 
#   plot_TargetsReached100 + Plot_Increase100 +
#   plot_TargetsReached_1000 + Plot_Increase_1000 +
#   plot_layout(ncol = 2) +
#   plot_annotation(tag_levels = 'a') +
#   theme(plot.tag = element_text(face = 'bold'))
  
ggsave("Figures/Targets_IncreaseServices.pdf",
         dpi = 1000, units = "cm", width = 16, height = 14) 

# Save layers
saveRDS(ntarget_reached_df_BioServ, "RDS/ntarget_reached_df_BioServ.rds")
saveRDS(ntarget_reached_df_BioServ_WDPA, "RDS/ntarget_reached_df_BioServ_WDPA.rds")
saveRDS(Increase_EcoServices_Prct, "RDS/Increase_EcoServices_Prct.rds")
saveRDS(Increase_EcoServices_WDPA_Prct, "RDS/Increase_EcoServices_WDPA_Prct.rds")
