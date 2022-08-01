#Alvise Dabalà
#2022-03-20

#Function to make a plot of the conservation planning results
#INPUT:
# - prior_sol: solution of prioritizr problem
# - col_border: variable to use to define the colour of the border or use "black"

fPlot_PrioritizrSolution <- function(sol_prior, col_border = NULL, WDPA = FALSE) {
  if(missing(col_border) & WDPA == FALSE) {
    # Read the map of the world
    world_map <- ne_countries(scale = "large", returnclass = "sf") %>%
      st_transform(crs = cCRS) %>% #I project the crs of the GMW to meters
      st_make_valid() #I make the shapefile valid
    
    #col_border <- enquo(col_border)
    
    sol_prior <- sol_prior %>% 
      mutate(solution_1 = case_when(solution_1 == 1 ~ "Selected",
                                    solution_1 == 0 ~ "Not Selected")) %>% 
      mutate(Protected = case_when(Protected == TRUE ~ "Existing Protected Areas",
                                   Protected == FALSE ~ " "))
    
    #Plot the solution
    Plot_ConsFeat <- ggplot() +
      geom_sf(data = world_map, colour ="grey30", fill = "grey50", size = 0.1) +
      geom_sf(data = sol_prior, aes(fill = as.factor(solution_1), colour = as.factor(Protected)), size = 0.01) +
      scale_fill_manual(values = c("Selected" = "#D81B60",
                                   "Not Selected" = "#1E88E5")) +
      scale_colour_manual(values = c("Existing Protected Areas" = "Black")) +
      theme_bw() +
      theme(legend.title = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) #+
    #geom_sf(data = GMW, colour = "palegreen3", fill = "palegreen3", alpha = 0.2, size = 0.1)
  } else if(missing(WDPA)) {
    # Read the map of the world
    world_map <- ne_countries(scale = "large", returnclass = "sf") %>%
      st_transform(crs = cCRS) %>% #I project the crs of the GMW to meters
      st_make_valid() #I make the shapefile valid
    
    col_border <- enquo(col_border)
    
    sol_prior <- sol_prior %>% 
      mutate(solution_1 = case_when(solution_1 == 1 ~ "Selected",
                                    solution_1 == 0 ~ "Not Selected"))
    
    #Plot the solution
    Plot_ConsFeat <- ggplot() +
      geom_sf(data = world_map, colour ="grey30", fill = "grey50", size = 0.1) +
      geom_sf(data = sol_prior, aes(fill = as.factor(solution_1), colour = !!col_border), size = 0.05) +
      scale_fill_manual(values = c("Selected" = "#D81B60",
                                   "Not Selected" = "#1E88E5")) +
      theme_bw() +
      scale_color_viridis_c() +
      theme(legend.title = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) #+
    #geom_sf(data = GMW, colour = "palegreen3", fill = "palegreen3", alpha = 0.2, size = 0.1)
  } else if(WDPA == TRUE) {
    # Read the map of the world
    world_map <- ne_countries(scale = "large", returnclass = "sf") %>%
      st_transform(crs = cCRS) %>% #I project the crs of the GMW to meters
      st_make_valid() #I make the shapefile valid
    
    #col_border <- enquo(col_border)
    
    sol_prior <- sol_prior %>% 
      mutate(solution_1 = case_when(solution_1 == 1 ~ "Selected",
                                    solution_1 == 0 ~ "Not Selected")) %>% 
      mutate(Protected = case_when(Protected == TRUE ~ "Existing Protected Areas",
                                   Protected == FALSE ~ " "))
    
    #Plot the solution
    Plot_ConsFeat <- ggplot() +
      geom_sf(data = world_map, colour ="grey30", fill = "grey50", size = 0.1) +
      geom_sf(data = sol_prior, aes(fill = as.factor(solution_1)), colour = NA, size = 0.01) +
      theme_bw() +
      scale_fill_manual(values = c("Selected" = "#D81B60",
                                   "Not Selected" = "#1E88E5",
                                   "Existing Protected Areas" = "#FFC107",
                                   " " = "transparent")) +
      geom_sf(data = sol_prior, aes(fill = as.factor(Protected)), colour = NA, size = 0.01) +
      theme(legend.title = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) #+
    #geom_sf(data = GMW, colour = "palegreen3", fill = "palegreen3", alpha = 0.2, size = 0.1)
  }
  
}
