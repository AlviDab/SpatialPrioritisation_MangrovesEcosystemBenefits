#Alvise Dabalà
#2022-05-17

#Function to plot the percent of PU selected using bigger PUs

fPlot_Rank <- function(sol_prtzr, Large_PUs = NULL, limits = NULL, palet = "viridis",
                       brk = c(1, 25, 50, 75, 100), lm = c(1, 100)) {
  # Read the map of the world
  world_map <- ne_countries(scale = "large", returnclass = "sf") %>%
    st_transform(crs = cCRS) %>% #I project the crs of the GMW to meters
    st_make_valid() #I make the shapefile valid
  
  if (missing(limits)) {
  #Geometry of the PUs not already protected
  Selected_PUs <- sol_prtzr
  
  #Mean value of selected PUs that intersect the bigger PU
  a <- st_intersects(Large_PUs, st_centroid(Selected_PUs)) %>% 
    lapply(function(x) {
      if(length(x) > 0) {
        Selected_PUs %>% 
          slice(x) %>%
          dplyr::select(rank) %>%
          st_drop_geometry() %>% 
          unlist() %>% 
          median()
      }
      else {
        x <- 0
      }
    }
    )
  
  #Transform list to columns of a dataframe
  Large_PUs$mean_rank <- unlist(a)
  
  Large_PUs_PP <- Large_PUs %>% 
    filter(mean_rank != 0)
  
  #Plot the solution
    Plot_ConsFeat <- ggplot() +
    geom_sf(data = world_map, colour ="grey50", fill = "grey70", size = 0.01) +
    geom_sf(data = Large_PUs_PP, aes(fill = mean_rank), colour = "black", size = 0.01) +
    scale_fill_viridis(option = palet, breaks = brk, limits = lm) +
    theme_bw(base_size = 8) +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          rect = element_rect(fill = "transparent"))
  } else {
    Plot_ConsFeat <- ggplot() +
      geom_sf(data = world_map, colour ="grey50", fill = "grey70", size = 0.05) +
      geom_sf(data = sol_prtzr, aes(fill = rank), colour = "black", size = 0.03) +
      scale_fill_viridis(option = palet, breaks = brk, limits = lm) +
      theme_bw(base_size = 8) +
      theme(legend.title = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none",
            rect = element_rect(fill = "transparent")) +
      coord_sf(xlim = c(limits$xmin, limits$xmax), 
               ylim = c(limits$ymin, limits$ymax), 
               expand = FALSE)
  }    #+
  #geom_sf(data = GMW, colour = "palegreen3", fill = "palegreen3", alpha = 0.2, size = 0.1)
}
