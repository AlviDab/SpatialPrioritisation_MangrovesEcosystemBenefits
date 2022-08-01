#Barplots

fplot_KernelPlots <- function(PUs, sol1, sol2, path, list_sol = FALSE) {
#   if(list_sol == TRUE) {
#   library(ggridges)
#   
#   sol <- sol[2:5]
#   list_titles <- list("Cost layer: fishing Intensity", "Cost layer: properties protected", "Cost layer: population protected", "Cost layer: carbon stored")
#   list_cost <- list("Fishing_Intensity", "TOT_STOCK", "POP", "Tot_Carbon")
#   list_x <- list("Fishing Intensity", "Properties protected", "Population protected", "Carbon stored")
#   list_xlab <- list(expression(Fishing~Intensity~log(fisher~days~km^{-2}~year^{-1})),
#                     expression(Properties~protected~log("$"~km^{-2})),
#                     expression(Population~protected~log(people~km^{-2})), 
#                     expression(Carbon~stored~(Mg~km^{-2})))
#   
#   kernel_plots <- lapply(seq_along(sol), function(x) {
#     
#     In_PA <- sol[[x]] %>%
#       filter(Protected == 1) %>% 
#       mutate(Selection = "Existing PAs")
#     
#     Selected_PA <- sol[[x]] %>%
#       filter(solution_1 == 1 & Protected != 1) %>% 
#       mutate(Selection = "Proposed PAs (50% budget)")
#     
#     PA_map <- In_PA %>% 
#       rbind(Selected_PA) %>% 
#       mutate(Fishing_Intensity = log(Fishing_Intensity + 1)) %>% 
#       mutate(TOT_STOCK = log(TOT_STOCK + 1)) %>% 
#       mutate(POP = log(POP + 1))
#     
#     #plot_PA <- lapply(seq_along(list_cost), function(y) {
#       # val <- x[,list_cost[[y]]] %>% 
#       #   st_drop_geometry() %>% 
#       #   filter(. != 0) %>% 
#       #   summarise(max_value = log(max(.)),
#       #          min_value = log(min(.)))
#      
#       plot_PA <- ggplot() +
#         geom_density_ridges2(data = PA_map, 
#                        aes(x =!!sym(list_cost[[x]]), 
#                            y = Selection,
#                            fill =  forcats::fct_rev(Selection)),
#                     #rel_min_height = 0.01,
#                     scale = 2) +
#         geom_vline(data=PA_map, aes(xintercept=!!sym(list_cost[[x]]), 
#                                     colour = forcats::fct_rev(Selection)), 
#                    linetype="dashed", size=0.5)
#         #ggtitle(list_titles[[x]]) +
#         xlab(list_xlab[[x]]) +
#         ylab("PUs selected") +
#         theme_classic(base_size = 15) +
#         #theme(axis.text.y=element_blank()) +
#           scale_fill_manual(values = c("Existing PAs" = "#022B3A",
#                                        "Proposed PAs (30% budget)" = "#1F7A8C",
#                                        "Proposed PAs (50% budget)" = "#BFDBF7"),
#                             name = "Status") +
#           scale_colour_manual(values = c("Existing PAs" = "#022B3A",
#                                          "Proposed PAs (30% budget)" = "#1F7A8C",
#                                          "Proposed PAs (50% budget)" = "#BFDBF7"),
#                               name = "Status")
#         # scale_x_continuous(trans="log10") +
#     #}
#     #)
#   
#   #list(plot_PA)  
#   }
#   )
#   
#   lapply(seq_along(kernel_plots), function(x) {
#     
#   kernel_plots[[x]]
#   
#    if(x == 1) {b = "B"}
#    if(x == 2) {b = "C"}
#    if(x == 3) {b = "D"}
#    if(x == 4) {b = "E"}
#    
#   ggsave(plot = kernel_plots[[x]], paste0(path, b, ".png"), width = 20, height = 15, 
#          dpi = 300, units = "cm", limitsize = FALSE)
# }
# )
#  } else if (list_sol == FALSE) {
    library(ggridges)
    
    list_titles <- list("Cost layer: fishing Intensity", "Cost layer: properties protected", "Cost layer: population protected", "Cost layer: carbon stored")
    list_cost <- list("Fishing_Intensity", "TOT_STOCK", "POP", "Tot_Carbon")
    list_x <- list("Fishing Intensity", "Properties protected", "Population protected", "Carbon stored")
    list_xlab <- list(expression(Fishing~Intensity~log(fisher~days~km^{-2}~year^{-1})),
                      expression(Properties~protected~log("$"~km^{-2})),
                      expression(Population~protected~log(people~km^{-2})), 
                      expression(Carbon~stored~(Mg~km^{-2})))
   
     kernel_plots <- lapply(1:4, function(x) {
      
       In_PA <- PUs %>%
        filter(Protected == 1) %>%
        mutate(Selection = "Existing protected areas")
      
      Selected_PA_30 <- sol1 %>%
        dplyr::select(!rank) %>% 
        st_as_sf %>% 
        mutate(Selection = "Priority areas biodiversity and ecosystem services")
      
      Selected_PA_50 <- sol2 %>%
        dplyr::select(!rank) %>% 
        st_as_sf %>%  
        mutate(Selection = "Priority areas biodiversity")
      
      PA_map <- In_PA %>% 
        rbind(Selected_PA_30) %>% 
        rbind(Selected_PA_50) %>% 
        mutate(Fishing_Intensity = log10(Fishing_Intensity + 1),
               TOT_STOCK = log10(TOT_STOCK + 1),
               POP = log10(POP + 1))
      
      mean_PA <- PA_map %>% 
        as_tibble() %>% 
        dplyr::select("Fishing_Intensity", "TOT_STOCK", "POP", "Tot_Carbon", "Selection") %>% 
        group_by(Selection) %>% 
        summarise(across(1:4, mean))
      
      #plot_PA <- lapply(seq_along(list_cost), function(y) {
      # val <- x[,list_cost[[y]]] %>% 
      #   st_drop_geometry() %>% 
      #   filter(. != 0) %>% 
      #   summarise(max_value = log(max(.)),
      #          min_value = log(min(.)))
      
      plot_PA <- ggplot() +
        geom_density_ridges2(data = PA_map, 
                             aes(x = !!sym(list_cost[[x]]), 
                                 y = Selection,
                                 fill =  factor(Selection, levels = c("Priority areas biodiversity and ecosystem services",
                                                                      "Priority areas biodiversity",
                                                                      "Existing protected areas"))),
                             colour =  "black",
                             alpha = 0.9,
                             size = 0.01,
                             #rel_min_height = 0.01,
                             scale = 2) +
        geom_vline(data = mean_PA , aes(xintercept = (!!sym(list_cost[[x]])), 
                                       colour = forcats::fct_rev(Selection)),
                   linetype = "dashed", size = 0.3, show.legend = FALSE, alpha = 1) +
        #ggtitle(list_titles[[x]]) +
        xlab(list_xlab[[x]]) +
        ylab("PUs selected") +
        theme_classic(base_size = 8
          ) +
        theme(axis.text.y=element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "none") +
        scale_y_discrete(breaks = NULL) +
        scale_fill_manual(values = c("Priority areas biodiversity and ecosystem services" = "#EDCF5A",
                                     "Priority areas biodiversity" = "#E57361",
                                     "Existing protected areas" = "#6C6D89")) +
        scale_colour_manual(values = c("Priority areas biodiversity and ecosystem services" = "#EDCF5A",
                                       "Priority areas biodiversity" = "#E57361",
                                       "Existing protected areas" = "#6C6D89")) #remove y axis labels
      # scale_x_continuous(trans="log10") +
      #}
      #)
      
      #list(plot_PA)  
    }
    )
     
     lapply(seq_along(kernel_plots), function(x) {
       
       kernel_plots[[x]]
       
       if(x == 1) {b = "Fish"}
       if(x == 2) {b = "Prop"}
       if(x == 3) {b = "Pop"}
       if(x == 4) {b = "Carbon"}
       
       ggsave(plot = kernel_plots[[x]], paste0(path, b, ".svg"), width = 7, height = 5.5, 
              units = "cm", dpi = 1000, limitsize = FALSE)
     }
     )
  }

