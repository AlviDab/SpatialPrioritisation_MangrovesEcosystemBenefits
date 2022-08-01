#Alvise Dabalà
#2022-03-22

#Function to plot the percent of PU selected using bigger PUs

fPlot_GlobalResults <- function(sol_prtzr, Large_PUs) {
  #Geometry of the selected PUs
  Selected_PUs <- sol_prtzr %>% 
    dplyr::select(solution_1) %>% 
    filter(solution_1 == "1")
  
  #Not selected PUs
  NotSelected_PUs <- sol_prtzr %>% 
    dplyr::select(solution_1) %>% 
    filter(solution_1 == "0")
  
  #Number of selected/not selected PUs that intersect the bigger PU
  a <- st_intersects(Large_PUs, st_centroid(Selected_PUs)) %>% 
    lapply(function(x) {
      if(length(x) > 0) {
        sol_prtzr %>% 
          slice(x) %>%
          dplyr::select(AreaGMWKm) %>%
          st_drop_geometry() %>% 
          unlist() %>% 
          mean()
      }
      else {
        x <- 0
      }
    }
      )
  
  b <- st_intersects(Large_PUs, st_centroid(NotSelected_PUs)) %>% 
    lapply(function(x) {
      if(length(x) > 0) {
        sol_prtzr %>% 
          slice(x) %>%
          dplyr::select(AreaGMWKm) %>%
          st_drop_geometry() %>% 
          unlist() %>% 
          mean()
      }
      else {
        x <- 0
      }
    }
    )
  
  #Transform list to columns of a dataframe
  Large_PUs$AreaSelected <- unlist(a)
  Large_PUs$AreaNotSelected <- unlist(b)
  
  #Calculate frequency percentage
  Large_PUs <- Large_PUs %>% 
    mutate(freqSelection = (AreaSelected/(AreaSelected + AreaNotSelected))*100) %>% 
    dplyr::filter(!is.na(freqSelection))
  
  # Read the map of the world
  world_map <- ne_countries(scale = "large", returnclass = "sf") %>%
    st_transform(crs = cCRS) %>% #I project the crs of the GMW to meters
    st_make_valid() #I make the shapefile valid
  
  #Plot the solution
  Plot_ConsFeat <- ggplot() +
    geom_sf(data = world_map, colour ="grey30", fill = "grey50", size = 0.1) +
    geom_sf(data = Large_PUs, aes(fill = freqSelection, colour = freqSelection), size = 0.01) +
    scale_fill_viridis_c() +
    theme_bw(base_size = 10) +
    scale_color_viridis_c() +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) #+
  #geom_sf(data = GMW, colour = "palegreen3", fill = "palegreen3", alpha = 0.2, size = 0.1)
}
