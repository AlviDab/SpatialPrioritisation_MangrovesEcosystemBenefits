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

#Calculate number of biodiversity features that reach the target increasing the area target

# Biodiversity and ecosystem services

# Calculate when each species reach the targets

targets_reached_final <- list()

targets_reached <- lapply(list(result_BioServ$rank, result_BioServ_WDPA$rank, 
                               result_Bio$rank, result_Bio_WDPA$rank 
),
                               function(x) {
  for(i in min(x):(max(x)-1)) {
    targets_reached <- PUs_NotSplitted %>% 
      dplyr::select(3:67) %>% #Select only species col
      bind_cols(rank = x) %>% #Add rank col
      st_drop_geometry() %>% 
      as_tibble() %>% 
      group_by(selected = rank <= i) %>% #Group <= i or >= 1
      summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop') %>% #sum each column value by group
      dplyr::select(!rank) %>% 
      pivot_longer(!selected, names_to = "names", values_to = "Area") %>%
      pivot_wider(names_from = selected, values_from = "Area") %>% 
      left_join(ConsFeatures_NotSplitted, by = "names") %>% #Add the minimum amount of service 
      mutate(amount_protected = `TRUE`/(`FALSE` + `TRUE`)) %>% 
      mutate(protected = case_when(amount_protected < amount ~ 0,
                                   TRUE ~ 1))

  targets_reached_final[[i]] <- targets_reached
  }
  targets_reached_final
  }
)

# Calculate the number of species that reach the targets for incremental rankings
ntarget_reached_df_BioServ <- tibble(prct = c(0, 100), reached = c(0, 100))

for(i in min(result_BioServ$rank):(max(result_BioServ$rank)-1)) {
  
  ntarget_reached_df_BioServ <- ntarget_reached_df_BioServ %>% 
    add_row(prct = i, reached = sum(targets_reached[[1]][[i]]$protected)/65*100)
}

ntarget_reached_df_BioServ_WDPA <- tibble(prct = c(100), reached = c(100))

for(i in min(result_BioServ_WDPA$rank):(max(result_BioServ_WDPA$rank)-1)) {
  
  ntarget_reached_df_BioServ_WDPA <- ntarget_reached_df_BioServ_WDPA %>% 
    add_row(prct = i, reached = sum(targets_reached[[2]][[i]]$protected)/65*100)
}

ntarget_reached_df_Bio <- tibble(prct = c(0, 100), reached = c(0, 100))

for(i in min(result_Bio$rank):(max(result_Bio$rank)-1)) {
  
  ntarget_reached_df_Bio <- ntarget_reached_df_Bio %>% 
    add_row(prct = i, reached = sum(targets_reached[[3]][[i]]$protected)/65*100)
}

ntarget_reached_df_Bio_WDPA <- tibble(prct = c(100), reached = c(100))

for(i in min(result_Bio_WDPA$rank):(max(result_Bio_WDPA$rank)-1)) {
  
  ntarget_reached_df_Bio_WDPA <- ntarget_reached_df_Bio_WDPA %>% 
    add_row(prct = i, reached = sum(targets_reached[[4]][[i]]$protected)/65*100)
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
    theme_bw(base_size = 8) +
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
fplot_targets(ntarget_reached_comparison_noWDPA) +
  geom_vline(xintercept = 28, colour = "black", size = 0.3, linetype = "dashed")

ggsave("Figures/TargetsReachedSpecies_noWDPA.pdf", 
       dpi = 1000, units = "cm", width = 8, height = 6)

#Plot WDPA
#Plot for species

fplot_targets(ntarget_reached_comparison_WDPA) +
  geom_vline(xintercept = 28, colour = "black", size = 0.3, linetype = "dashed")

ggsave("Figures/TargetsReachedSpecies_WDPA.pdf", 
       dpi = 1000, units = "cm", width = 8, height = 6)

################################################################################
#Increase benefits for increasing targets

Total_Services <- PUs %>% 
  as_tibble %>% 
  summarise(Fishing = sum(Fishing_Intensity*AreaGMWKm), 
            People = sum(POP*AreaGMWKm),
            Properties = sum(TOT_STOCK*AreaGMWKm),
            Carbon = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm = sum(AreaGMWKm))

PA_Services <- PUs %>% 
  as_tibble %>% 
  filter(Protected == 1) %>% 
  summarise(Fishing = sum(Fishing_Intensity*AreaGMWKm), 
            People = sum(POP*AreaGMWKm),
            Properties = sum(TOT_STOCK*AreaGMWKm),
            Carbon = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm = sum(AreaGMWKm))

Increase_EcoServices <- c()

for (i in 1:max(result_BioServ$rank)) {
  EcoServ <- result_BioServ %>% 
    filter(rank <= i) %>% 
    as_tibble %>% 
    summarise(Fishing = sum(Fishing_Intensity*AreaGMWKm), 
              People = sum(POP*AreaGMWKm),
              Properties = sum(TOT_STOCK*AreaGMWKm),
              Carbon = sum(Tot_Carbon*AreaGMWKm),
              AreaGMWKm = sum(AreaGMWKm))
  
  Increase_EcoServices <- Increase_EcoServices %>% 
    rbind(EcoServ)
}

rm(EcoServ)

Increase_EcoServices <- Increase_EcoServices %>% 
  mutate(prct = as.numeric(rownames(.)))

Increase_EcoServices_WDPA <- c()

for (i in 1:max(result_BioServ_WDPA$rank)) {
  EcoServ <- result_BioServ_WDPA %>% 
    filter(rank <= i) %>% 
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
  mutate(prct = 27.99)

Increase_EcoServices_WDPA <- Increase_EcoServices_WDPA %>% 
  rbind(PA_Services) %>% 
  filter(AreaGMWKm != 0)

Increase_EcoServices_Prct <- Increase_EcoServices %>% 
  summarise(Fishing = Fishing/Total_Services$Fishing, 
            People = People/Total_Services$People,
            Properties = Properties/Total_Services$Properties,
            Carbon = Carbon/Total_Services$Carbon,
            prct = prct)
  
Increase_EcoServices_WDPA_Prct <- Increase_EcoServices_WDPA %>% 
  summarise(Fishing = Fishing/Total_Services$Fishing, 
            People = People/Total_Services$People,
            Properties = Properties/Total_Services$Properties,
            Carbon = Carbon/Total_Services$Carbon,
            prct = prct)

#Percentage of each biodiversity feature protected for increasing targets
fPlot_EcoServ_Increase <- function(Increase, x) {
  
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
  ggplot(data = long_Increase, aes(x = prct, y = value, colour = factor(benefit, levels = c("Fishing", "People", "Properties", "Carbon")))) +
    geom_line(size = 0.5) +
    geom_point(size = 0.5) +
    scale_color_manual(name = "Features",
                       labels = c(c("Fishing",
                                    "People", 
                                    "Properties",
                                    "Carbon")),
                       values = c("#1E88E5","#FFC107","#D81B60", "#004D40")) +
    xlab("Mangroves selected in priority areas (%)") +
    ylab("Proportion of service protected") +
    theme_bw(base_size = 8) +
    theme(legend.position = "none",
          legend.title = element_blank()#,
          #legend.background = element_rect(fill="NA", size=0.5, linetype="solid", colour ="NA")
          ) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 101)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1.01)) +
    geom_vline(xintercept = PA_Services$prct, colour = "black", size=0.3, linetype = "dashed")
  
    ggsave(paste0("Figures/", x),
        dpi = 1000, units = "cm", width = 8, height = 6) 
}

# Ecosystem services and biodiversity

fPlot_EcoServ_Increase(Increase_EcoServices_Prct, "/Increase_Benefits_BioServ.pdf")

# Ecosystem services and biodiversity WDPA

fPlot_EcoServ_Increase(Increase_EcoServices_WDPA_Prct, "/Increase_Benefits_BioServ_WDPA.pdf")

# Save layers
saveRDS(ntarget_reached_df_BioServ, "RDS/ntarget_reached_df_BioServ.rds")
saveRDS(ntarget_reached_df_BioServ_WDPA, "RDS/ntarget_reached_df_BioServ_WDPA.rds")
saveRDS(Increase_EcoServices_Prct, "RDS/Increase_EcoServices_Prct.rds")
saveRDS(Increase_EcoServices_WDPA_Prct, "RDS/Increase_EcoServices_WDPA_Prct.rds")