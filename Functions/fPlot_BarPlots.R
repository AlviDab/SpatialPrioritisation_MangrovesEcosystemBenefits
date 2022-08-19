#Author: Alvise Dabalà
#Function to produce the barplots

#Inputs:
# - PUs: planning units shapefile 
# - sol1: planning units selected when expanding protection to 30%
# - sol2: planning units selected when expanding protection to 50%

fplot_BarPlots <- function(PUs, sol1, sol2) {
  
  list_titles <- list("Cost layer: fishing Intensity", "Cost layer: properties protected", "Cost layer: population protected", "Cost layer: carbon stored")
  list_cost <- list("Fishing_Intensity", "TOT_STOCK", "POP", "Tot_Carbon")
  list_x <- list("Fishing Intensity", "Properties protected", "Population protected", "Carbon stored")
  list_ylab <- list(expression(Mean~fishing~Intensity~(fisher~days~km^{-2}~year^{-1})),
                    expression(Mean~properties~protected~("$"~km^{-2})),
                    expression(Mean~population~protected~(people~km^{-2})), 
                    expression(Mean~carbon~stored~(Mg~km^{-2})))
  
  barplots <- lapply(1:4, function(x) {
    
    In_PA <- PUs %>%
      filter(Protected == 1) %>%
      mutate(Selection = "Existing protected areas")
    
    Selected_PA_30 <- sol1 %>%
      filter(Protected != 1) %>% 
      dplyr::select(!rank) %>% 
      st_as_sf %>% 
      mutate(Selection = "Priority areas biodiversity and ecosystem services")
    
    Selected_PA_50 <- sol2 %>%
      filter(Protected != 1) %>% 
      dplyr::select(!rank) %>% 
      st_as_sf %>%  
      mutate(Selection = "Priority areas biodiversity")
    
    PA_map <- In_PA %>% 
      rbind(Selected_PA_30) %>% 
      rbind(Selected_PA_50)
    
    mean_PA <- PA_map %>% 
      as_tibble() %>% 
      dplyr::select("Fishing_Intensity", "TOT_STOCK", "POP", "Tot_Carbon", "Selection", "AreaGMWKm") %>% 
      group_by(Selection) %>% 
      summarise(Fishing_Intensity = sum(Fishing_Intensity*AreaGMWKm)/(sum(AreaGMWKm)),
                TOT_STOCK = sum(TOT_STOCK*AreaGMWKm)/(sum(AreaGMWKm)),
                POP = sum(POP*AreaGMWKm)/(sum(AreaGMWKm)),
                Tot_Carbon = sum(Tot_Carbon*AreaGMWKm)/(sum(AreaGMWKm)))
    
    sd_PA <- PA_map %>% 
      as_tibble() %>% 
      dplyr::select("Fishing_Intensity", "TOT_STOCK", "POP", "Tot_Carbon", "Selection") %>% 
      group_by(Selection) %>% 
      summarise(across(1:4, sd),
                n = n()) %>% 
      rename_with(~paste0(., "_sd"), Fishing_Intensity:Tot_Carbon)
    
    mean_PA <- mean_PA %>% 
      left_join(sd_PA, by = "Selection")
    
    mean_PA
    
    mean_PA <- mean_PA %>% 
      mutate(Fishing_Intensity_margin = qt(.975,df = n-1)*Fishing_Intensity_sd/sqrt(n),
             TOT_STOCK_margin = qt(.975,df = n-1)*TOT_STOCK_sd/sqrt(n),
             POP_margin = qt(.975,df = n-1)*POP_sd/sqrt(n),
             Tot_Carbon_margin = qt(.975,df = n-1)*Tot_Carbon_sd/sqrt(n))
    
    scientific10 <- function(x) {
      parse(text = gsub("e+", " %*% 10^", scales::scientific_format()(x)))
    }
    
    max_val <- c(2500, 7.5e+05, 300, 6)
    
    if(x == 2) {plot_PA <- ggplot(mean_PA, aes(x = Selection, y = !!sym(list_cost[[x]]), fill = Selection)) +
      geom_col(colour =  "black", alpha = 1) +
      # geom_errorbar(aes(ymin=!!sym(list_cost[[x]])-!!sym(paste0(list_cost[[x]], "_margin")), 
      #                   ymax=!!sym(list_cost[[x]])+!!sym(paste0(list_cost[[x]], "_margin")))) +
      ylab(list_ylab[[x]]) +
      scale_fill_manual(values = c("Priority areas biodiversity and ecosystem services" = "#EDCF5A",
                                                                  "Priority areas biodiversity" = "#E57361",
                                                                "Existing protected areas" = "#6C6D89")) +
      scale_y_continuous(limits = c(0, max_val[[x]]), 
                         labels = scientific10) +
      theme_bw(base_size = 6.5) +
      theme(axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.position = "none") +
      scale_x_discrete(breaks = NULL)}
    else {
      plot_PA <- ggplot(mean_PA, aes(x = Selection, y = !!sym(list_cost[[x]]), fill = Selection)) +
        geom_col(colour =  "black", alpha = 1) +
        # geom_errorbar(aes(ymin=!!sym(list_cost[[x]])-!!sym(paste0(list_cost[[x]], "_margin")), 
        #                   ymax=!!sym(list_cost[[x]])+!!sym(paste0(list_cost[[x]], "_margin")))) +
        ylab(list_ylab[[x]]) +
        scale_fill_manual(values = c("Priority areas biodiversity and ecosystem services" = "#EDCF5A",
                                     "Priority areas biodiversity" = "#E57361",
                                     "Existing protected areas" = "#6C6D89")) +
        scale_y_continuous(limits = c(0, max_val[[x]])) +
        theme_bw(base_size = 6.5) +
        theme(axis.title.x = element_blank(),
              legend.title = element_blank(),
              legend.position = "none") +
        scale_x_discrete(breaks = NULL) 
    }
    
    #   geom_density_ridges2(data = PA_map, 
    #                        aes(x = !!sym(list_cost[[x]]), 
    #                            y = Selection,
    #                            fill =  factor(Selection, levels = c("Priority areas biodiversity and ecosystem services",
    #                                                                 "Priority areas biodiversity",
    #                                                                 "Existing protected areas"))),
    #                        colour =  "black",
    #                        alpha = 0.9,
    #                        size = 0.01,
    #                        #rel_min_height = 0.01,
    #                        scale = 2) +
    #   #ggtitle(list_titles[[x]]) +
    #   xlab(list_xlab[[x]]) +
    #   ylab("PUs selected") +
    #   theme_classic(base_size = 8
    #   ) +
    #   theme(axis.text.y=element_blank(),
    #         axis.title.y = element_blank(),
    #         legend.title = element_blank(),
    #         legend.position = "none") +
    #   scale_y_discrete(breaks = NULL) +
    #   scale_fill_manual(values = c("Priority areas biodiversity and ecosystem services" = "#EDCF5A",
    #                                "Priority areas biodiversity" = "#E57361",
    #                                "Existing protected areas" = "#6C6D89")) +
    #   scale_colour_manual(values = c("Priority areas biodiversity and ecosystem services" = "#EDCF5A",
    #                                  "Priority areas biodiversity" = "#E57361",
    #                                  "Existing protected areas" = "#6C6D89")) #remove y axis labels
    # # scale_x_continuous(trans="log10") +
    # #}
    # #)
    
    #list(plot_PA)  
  }
  )
  
  return(barplots)
}

