#Author: Alvise Dabalà
#Function to calculate statistic by country and by region of the planning units selected
#as priority areas

#Input: 
# - result <sf>: result of the prioritization
# - n <numeric>: maximum rank we consider

################################################################################
###Statistics by country and region
##Priority areas

#n is the rank
source("Functions/fSelect_PriorityAreasProtected.r") #Selected priority areas

fStat_CountryContinent <- function(result, n, IUCN = "I-IV") { #Percentage of mangroves in priority areas (top 10%)
  #Total mangroves by country
  total_country <- PUs %>% 
    as_tibble() %>% 
    group_by(country) %>% #group protected areas by country
    summarise(total_area = sum(AreaGMWKm)) #sum the area of GMW
  
  ##Percentage of top-x priority areas by country
  priority_area_cover_country <- result %>% 
    as_tibble() %>% 
    filter(rank <= n) %>% #PUs con rank <= n 
    group_by(country) %>% #Group by country 
    summarise(priority_area_cover = sum(AreaGMWKm)) %>% #sum of the area of the priority areas
    full_join(total_country,  by = "country") %>% #left join the total mangrove area by country
    mutate(priority_area_cover = replace_na(priority_area_cover, 0)) %>% 
    mutate(perc_priority_area_country = priority_area_cover/total_area*100, #percentage of priority areas on the total mangrove area in the country
           total_perc_priority_area_country = priority_area_cover/sum(priority_area_cover)*100) #percentage of priority areas cover over the total priority areas cover
  
  country_stat <- priority_area_cover_country
  
  ## by continent
  
  # Area by continent
  total_continent <- PUs %>% 
    as_tibble() %>% 
    group_by(continent) %>% 
    summarise(total_area = sum(AreaGMWKm)) %>% #Total area by continent
    mutate(percentage_area = total_area/sum(total_area)*100) #Percentage of the total area by continent
  
  # priority areas by continent
  priority_area_cover_continent <- result %>% 
    as_tibble() %>% 
    filter(rank <= n) %>% #Select only PUs <= rank
    group_by(continent) %>% 
    summarise(priority_area_cover = sum(AreaGMWKm)) %>% #Area of priority areas in the continent
    full_join(total_continent,  by = "continent") %>% 
    mutate(priority_area_cover = replace_na(priority_area_cover, 0)) %>% #replace all the NAs of priority area cover
    mutate(perc_priority_area_continent = priority_area_cover/total_area*100, #Percentage of priority areas of that continent
           total_perc_priority_area_continent = priority_area_cover/sum(priority_area_cover)*100) # Percentage of priority areas in the continent respect to the total priority areas area 
  
  continent_stat <- priority_area_cover_continent
  
  ################################################################################
  ### Mangroves values
  
  ## Ecosystem services per square km at country scale → what are the countries selected for higher services?
  services_country <- result %>% 
    as_tibble %>% 
    filter(rank <= n) %>% #Select only those with rank <= n
    group_by(country) %>% 
    summarise(n_PUs = n(), #Number of planning units
              Fishing = sum(Fishing_Intensity*AreaGMWKm), #Total fishing intensity
              Population = sum(POP*AreaGMWKm), #Total population
              Properties = sum(TOT_STOCK*AreaGMWKm), #Total number of properties
              Carbon = sum(Tot_Carbon*AreaGMWKm), #Total carbon
              Fishing_km2 = sum(Fishing_Intensity*AreaGMWKm)/sum(AreaGMWKm), #Fishing intensity/km²
              Population_km2 = sum(POP*AreaGMWKm)/sum(AreaGMWKm), #Population/Km²
              Properties_km2 = sum(TOT_STOCK*AreaGMWKm)/sum(AreaGMWKm), #Stock/Km²
              Carbon_km2 = sum(Tot_Carbon*AreaGMWKm)/sum(AreaGMWKm), #TotalCarbon/Km²
              continent = first(continent)) #Name of the continent
  
  country_stat <- country_stat %>% 
    left_join(services_country, by = 'country') #Left join the calculations by country
  
  services_continent <- result %>% 
    as_tibble %>% 
    filter(rank <= n) %>% #Filter all the PUs with rank <= n
    group_by(continent) %>% #Group the planning units by continent
    summarise(n_PUs = n(), #Number of planning units 
              Fishing = sum(Fishing_Intensity*AreaGMWKm), #Total fishing intensity
              Population = sum(POP*AreaGMWKm), #Total population
              Properties = sum(TOT_STOCK*AreaGMWKm), #Total properties
              Carbon = sum(Tot_Carbon*AreaGMWKm), #Total carbon
              Fishing_km2 = sum(Fishing_Intensity*AreaGMWKm)/sum(AreaGMWKm), #Fishing intensity/km²
              Population_km2 = sum(POP*AreaGMWKm)/sum(AreaGMWKm), #Population/Km²
              Properties_km2 = sum(TOT_STOCK*AreaGMWKm)/sum(AreaGMWKm), #Stock/Km²
              Carbon_km2 = sum(Tot_Carbon*AreaGMWKm)/sum(AreaGMWKm), #TotalCarbon/Km²
              continent = first(continent)) #Continent name
  
  continent_stat <- continent_stat %>% 
    left_join(services_continent)
  
  ## Number of mangroves species by country?
  n_species_by_country <- PUs_NotSplitted %>% 
    as_tibble %>% 
    dplyr::select(c(names(species), country)) %>% #Select all the column of the species and the country column
    group_by(country) %>% #group the results by country
    summarise_all(sum) %>% #Summarise doing a sum of all the values by country
    mutate(nSpecies = rowSums(.!=0)) %>% #do a rowsum → all the cells that are 0 = the species is not present
    dplyr::select(country, nSpecies) #Select country and number of species columns
  
  country_stat <- country_stat %>% 
    left_join(n_species_by_country, by = 'country') #left join the number of species by country
  
  ## Same by continent
  n_species_by_continent <- PUs_NotSplitted %>% 
    as_tibble %>% 
    dplyr::select(c(names(species), continent)) %>% #select the species and the continent columns
    group_by(continent) %>% #group by continent
    summarise_all(sum) %>% #sum all the values by continent
    mutate(nSpecies = rowSums(.!=0)) %>% #RowSums of all the values that are not 0 → number of species that are in that continent
    dplyr::select(continent, nSpecies) #Select continenet and the number of species
  
  continent_stat <- continent_stat %>% 
    left_join(n_species_by_continent) #Left join the number of species by continent
  
  if(IUCN == "I-IV") {
  
    ## Percentage of total mangroves protected by country/continent
    protection_country <- PUs %>% 
      as_tibble() %>% #Transform to tibble
      filter(Protected == TRUE) %>% #Select only PUs currently protected
      group_by(country) %>% #group by country
      summarise(tot_mangrove_protected = sum(AreaGMWKm)) #sum of mangroves by country considered already protected
    
    country_stat <- country_stat %>% 
      left_join(protection_country, by = 'country') %>% #protect countries by country
      mutate(prct_tot_mangrove_protected = tot_mangrove_protected/total_area) #calculate the percentage of total mangrove areas already protected
    
    ## Percentage of total mangroves protected by continent/continent
    protection_continent <- PUs %>% 
      as_tibble() %>% 
      filter(Protected == "TRUE") %>% #Only PUs currently protected 
      group_by(continent) %>% #group by continent
      summarise(tot_mangrove_protected = sum(AreaGMWKm)) #Total of mangrove areas protected
    
    continent_stat <- continent_stat %>% 
      left_join(protection_continent, by = 'continent') %>% #Left join the protected areas by continent
      mutate(prct_tot_mangrove_protected = tot_mangrove_protected/total_area) #calculate the percentage of protected areas by continent
    
      ################################################################################
      ### Comparison priority protected and non protected areas
      ## Percentage priority areas protected
      
      priority_areas_protected <- f_PriorityAreasProtected(result, n, IUCN = "I-IV") #Calculate the priority areas stats
      
      # By country
      country_stat <- country_stat %>%
        left_join(priority_areas_protected[[1]], by = 'country') %>% #left join the stats on priority areas
        mutate(across(where(is.numeric), ~replace_na(., 0))) #replace all the NAs with zeros
      
      # By region
      continent_stat <- continent_stat %>%
        left_join(priority_areas_protected[[2]], by = 'continent') %>% #left join the stats on priority areas
        replace(is.na(.), 0) #replace all the NAs with zeros
      
      list(country_stat, continent_stat) #list of the stats at country and continent scale
  }
  
  if(IUCN == "All") {
    ## Percentage of total mangroves Protected_I_VI by country/continent
    protection_country <- PUs %>% 
      as_tibble() %>% #Transform to tibble
      filter(Protected_I_VI == TRUE) %>% #Select only PUs currently Protected_I_VI
      group_by(country) %>% #group by country
      summarise(tot_mangrove_Protected_I_VI = sum(AreaGMWKm)) #sum of mangroves by country considered already Protected_I_VI
    
    country_stat <- country_stat %>% 
      left_join(protection_country, by = 'country') %>% #protect countries by country
      mutate(prct_tot_mangrove_Protected_I_VI = tot_mangrove_Protected_I_VI/total_area) #calculate the percentage of total mangrove areas already Protected_I_VI
    
    ## Percentage of total mangroves Protected_I_VI by continent/continent
    protection_continent <- PUs %>% 
      as_tibble() %>% 
      filter(Protected_I_VI == "TRUE") %>% #Only PUs currently Protected_I_VI 
      group_by(continent) %>% #group by continent
      summarise(tot_mangrove_Protected_I_VI = sum(AreaGMWKm)) #Total of mangrove areas Protected_I_VI
    
    continent_stat <- continent_stat %>% 
      left_join(protection_continent, by = 'continent') %>% #Left join the Protected_I_VI areas by continent
      mutate(prct_tot_mangrove_Protected_I_VI = tot_mangrove_Protected_I_VI/total_area) #calculate the percentage of Protected_I_VI areas by continent
    
    ################################################################################
    ### Comparison priority Protected_I_VI and non Protected_I_VI areas
    ## Percentage priority areas Protected_I_VI
    
    priority_areas_Protected_I_VI <- f_PriorityAreasProtected(result, n, IUCN = "All") #Calculate the priority areas stats
    
    # By country
    country_stat <- country_stat %>%
      left_join(priority_areas_Protected_I_VI[[1]], by = 'country') %>% #left join the stats on priority areas
      mutate(across(where(is.numeric), ~replace_na(., 0))) #replace all the NAs with zeros
    
    # By region
    continent_stat <- continent_stat %>%
      left_join(priority_areas_Protected_I_VI[[2]], by = 'continent') %>% #left join the stats on priority areas
      replace(is.na(.), 0) #replace all the NAs with zeros
    
    list(country_stat, continent_stat) #list of the stats at country and continent scale    
  }
  
  return(list(country_stat, continent_stat))
}
