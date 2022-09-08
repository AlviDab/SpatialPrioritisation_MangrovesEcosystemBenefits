#Author: Dabalà Alvise
#Function to plot the values reported in a planning unit
#Input: 
# - x <sf>: planning units shapefile;
# - col_name <string>: name of the column that you want to plot the values of;
# - scale_fill <string>: viridis category used, default is "turbo";
# - logarithmic <logical>:if true, data are transformed to log10 scale;
# - WDPA <logical>: if true, already protected areas are filled with a different colour;
# - large_PUs <logical>: if true, data are aggregated at large_PUs scale

fPlot_PUsValues <- function(x, col_name, scale_fill = "turbo", logarithmic = FALSE, 
                            WDPA = FALSE, large_PUs = FALSE) {
  world_map <- ne_countries(scale = "large", returnclass = "sf") %>%
    st_transform(crs = cCRS) %>% #I project the crs of the GMW to meters
    st_make_valid() #I make the shapefile valid
  
  if (logarithmic == FALSE & WDPA == FALSE & large_PUs == FALSE) {
  
    #Plot the solution
    Plot_ConsFeat <- ggplot() +
      geom_sf(data = world_map, colour ="grey50", fill = "grey70", size = 0.1) +
      geom_sf(data = x, aes(fill = !!sym(col_name)), colour = NA, size = 0.01) +
      theme_bw() +
      scale_fill_viridis(option = scale_fill) +
      theme(legend.title = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            rect = element_rect(fill = "transparent")) 
  
    Plot_ConsFeat
  }
  else if (logarithmic == FALSE & WDPA == TRUE & large_PUs == FALSE) {
    library(ggnewscale)
    
    x <- x %>% 
      mutate(Protected = case_when(Protected == TRUE ~ "Existing Protected Areas",
                                   Protected == FALSE ~ " "))
    
    #Plot the solution
    Plot_ConsFeat <- ggplot() +
      geom_sf(data = world_map, colour ="grey50", fill = "grey70", size = 0.1) +
      geom_sf(data = x, aes(fill = !!sym(col_name)), colour = NA, size = 0.01) +
      theme_bw(base_size = 8) +
      scale_fill_viridis(option=scale_fill) +
      theme(legend.title = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            rect = element_rect(fill = "transparent")) +
      new_scale_fill() +
      geom_sf(data = x, aes(fill = Protected), colour = NA, size = 0.01) +
      scale_fill_manual(name = "",
                        values = c("Existing Protected Areas" = "#BFC9E2",
                                   " " = "transparent"))
    
    Plot_ConsFeat
  }
  else if (logarithmic == TRUE & large_PUs == FALSE) {
    world_map <- ne_countries(scale = "large", returnclass = "sf") %>%
      st_transform(crs = cCRS) %>% #I project the crs of the GMW to meters
      st_make_valid() #I make the shapefile valid
    
    #Plot the solution
    Plot_ConsFeat <- ggplot() +
      geom_sf(data = world_map, colour ="grey50", fill = "grey70", size = 0.1) +
      geom_sf(data = x, aes(fill = log10(!!sym(col_name))), colour = NA, size = 0.01) +
      theme_bw(base_size = 8) +
      scale_fill_viridis(option=scale_fill) +
      theme(legend.title = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            rect = element_rect(fill = "transparent")) 
    
    Plot_ConsFeat 
  }
  else if (large_PUs == TRUE) {
    world_map <- ne_countries(scale = "large", returnclass = "sf") %>%
      st_transform(crs = cCRS) %>% #I project the crs of the GMW to meters
      st_make_valid() #I make the shapefile valid
    
    a <- st_intersects(Large_PUs, st_centroid(x)) %>% 
      lapply(function(y) {
        if(length(y) > 0) {
          x %>% 
            slice(y) %>%
            dplyr::select(!!sym(col_name)) %>%
            st_drop_geometry() %>% 
            unlist() %>% 
            mean()
        }
          else {
            y <- NA
          }
      }
      ) 
    
    Large_PUs <- Large_PUs %>% 
      mutate(!!sym(col_name) := unlist(a)) %>% 
      filter(!is.na(!!sym(col_name))) 
    
    if(logarithmic == TRUE) {
      
      Large_PUs <- Large_PUs %>% 
        mutate(!!sym(col_name) := !!sym(col_name) + 1)
      
    #Plot the solution
      Plot_ConsFeat <- ggplot() +
        geom_sf(data = world_map, colour ="grey50", fill = "grey70", size = 0.1) +
        geom_sf(data = Large_PUs, aes(fill = log10(!!sym(col_name))), colour = NA, size = 0.01) +
        theme_bw(base_size = 8) +
        scale_fill_viridis(option = scale_fill) +
        theme(legend.title = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              rect = element_rect(fill = "transparent")) 
    } else if (logarithmic == FALSE) {
      Plot_ConsFeat <- ggplot() +
        geom_sf(data = world_map, colour ="grey50", fill = "grey70", size = 0.1) +
        geom_sf(data = Large_PUs, aes(fill = !!sym(col_name)), colour = NA, size = 0.01) +
        theme_bw(base_size = 8) +
        scale_fill_viridis(option = scale_fill) +
        theme(legend.title = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              rect = element_rect(fill = "transparent")) 

    }
  }
}
