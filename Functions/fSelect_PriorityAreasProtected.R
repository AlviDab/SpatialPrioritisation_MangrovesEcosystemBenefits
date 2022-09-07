#Author: Alvise Dabalà
#Function to calculate how much of the areas are protected by country, region and in total

#Input: 
#- result <sf>: result of the prioritization with a column reporting the rank of the results
#- n <numeric>: is the rank for which we want to analyse the result

f_PriorityAreasProtected <- function(result, n) {
  
  # Mean priority by country
  priority_noWDPA <- result %>% 
    group_by(country) %>% #group by country
    summarise(mean_priority = mean(rank), #calculate the mean rank
              median_priority = median(rank)) #calculate the median rank
  
  # Area of priority mangroves outside of protected areas
  priority_protected <- result %>%
    as_tibble() %>% 
    filter(rank <= n) %>% #filter all PUs <= that priority
    filter(Protected == TRUE) %>% #filter all the PUs already protected
    group_by(country) %>% #group by country
    summarise(priority_protected = sum(AreaGMWKm)) #calculate the extent priority areas currently protected
  
  priority_notprotected <- result %>%
    as_tibble() %>% 
    filter(rank <= n) %>%  #filter all the PUs <= n
    filter(Protected == FALSE) %>% #Keep only not protected PUs
    group_by(country) %>% #group by country
    summarise(priority_notprotected = sum(AreaGMWKm)) #Calculate the extent of not protected priority areas
  
  priority_perc_protected_country <- priority_protected %>% 
    full_join(priority_notprotected, by = "country") %>% 
    replace(is.na(.), 0) %>% #replace NAs with 0
    mutate(perc_priority_protected = priority_protected/(priority_protected + priority_notprotected)*100) #Calculate percentage of priority protected 
  
  #Not by country
  priority_perc_protected_country %>% 
    summarise(across(!country, sum)) %>% #Calculate the amount of priority protected not by country
    mutate(perc_priority_protected = priority_protected/(priority_protected + priority_notprotected)*100)

  region_country <- PUs %>% 
    as_tibble() %>% 
    dplyr::select(country, continent) %>% 
    distinct()
    
  priority_perc_protected_region <- priority_perc_protected_country %>% 
    left_join(region_country, by = "country") %>% #left join column of continent and country
    group_by(continent) %>% #group by continent
    summarise(across(!country, sum)) %>% #make sum for all the columns that are not the country column
    mutate(perc_priority_protected = priority_protected/(priority_protected + priority_notprotected)*100) #calculate the percentage of priority areas protected by continent
  
  #Not by country
  priority_perc_protected_tot <- priority_perc_protected_country %>%
    summarise(across(!country, sum)) %>% #summarise across all the columns that are not country column
    mutate(perc_priority_protected = priority_protected/(priority_protected + priority_notprotected)*100) #total priority areas already protected
  
  priority <- list(priority_perc_protected_country,  priority_perc_protected_region, priority_perc_protected_tot) #list outputs
}
  
  