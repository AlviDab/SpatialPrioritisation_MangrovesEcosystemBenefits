#Author: Alvise Dabalà
#Function to calculate how much of the areas are protected by country, region and in total

#Input: 
#- result <sf>: result of the prioritization with a column reporting the rank of the results
#- n <numeric>: is the rank for which we want to analyse the result

f_PriorityAreasProtected <- function(result, n) {
  
  # Mean priority by country
  priority_noWDPA <- result %>% 
    group_by(country) %>% 
    summarise(mean_priority = mean(rank),
              median_priority = median(rank))
  
  # Area of priority mangroves (top 10%) outside of protected areas
  priority_protected <- result %>%
    as_tibble() %>% 
    filter(rank < n + 1) %>% 
    filter(Protected == TRUE) %>% 
    group_by(country) %>% 
    summarise(priority_protected = sum(AreaGMWKm))
  
  priority_notprotected <- result %>%
    as_tibble() %>% 
    filter(rank < n + 1) %>% 
    filter(Protected == FALSE) %>% 
    group_by(country) %>% 
    summarise(priority_notprotected = sum(AreaGMWKm))
  
  priority_perc_protected <- priority_notprotected %>% 
    full_join(priority_protected, by = "country") %>% 
    replace(is.na(.), 0) %>% 
    mutate(perc_priority_protected = priority_protected/(priority_protected + priority_notprotected)*100)
  
  #Not by country
  priority_perc_protected %>% 
    summarise(across(!country, sum)) %>% 
    mutate(perc_priority_protected = priority_protected/(priority_protected + priority_notprotected)*100)
  
  # Area of priority mangroves (top 28%) outside of protected areas
  priority_protected <- result %>%
    as_tibble() %>% 
    filter(rank < n + 1) %>% 
    filter(Protected == TRUE) %>% 
    group_by(country) %>% 
    summarise(priority_protected = sum(AreaGMWKm))
  
  priority_notprotected <- result %>%
    as_tibble() %>% 
    filter(rank < n + 1) %>% 
    filter(Protected == FALSE) %>% 
    group_by(country) %>% 
    summarise(priority_notprotected = sum(AreaGMWKm))
  
  priority_perc_protected_country <- priority_protected %>% 
    full_join(priority_notprotected, by = "country") %>% 
    replace(is.na(.), 0) %>% 
    mutate(perc_priority_protected = priority_protected/(priority_protected + priority_notprotected)*100)
  
  region_country <- PUs %>% 
    as_tibble() %>% 
    dplyr::select(country, continent) %>% 
    distinct()
    
  priority_perc_protected_region <- priority_perc_protected_country %>% 
    left_join(region_country, by = "country") %>% 
    group_by(continent) %>% 
    summarise(across(!country, sum)) %>% 
    mutate(perc_priority_protected = priority_protected/(priority_protected + priority_notprotected)*100)
  
  #Not by country
  priority_perc_protected_tot <- priority_perc_protected_country %>% 
    summarise(across(!country, sum)) %>% 
    mutate(perc_priority_protected = priority_protected/(priority_protected + priority_notprotected)*100)
  
  priority <- list(priority_perc_protected_country,  priority_perc_protected_region, priority_perc_protected_tot)
}
  
  