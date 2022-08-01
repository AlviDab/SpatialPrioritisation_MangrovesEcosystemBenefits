################################################################################
###Statistics by country and region
##Priority areas

#n is the rank

fStat_CountryContinent_PAs <- function(PUs) { ##Percentage of mangroves in priority areas (top 10%)
  #Total mangroves by country
  total_country <- PUs %>% 
    as_tibble() %>% 
    group_by(country) %>% 
    summarise(total_area = sum(AreaGMWKm))
  
  ## by continent
  
  # Area by continent
  total_continent <- PUs %>% 
    as_tibble() %>% 
    group_by(continent) %>% 
    summarise(total_area = sum(AreaGMWKm)) %>%
    mutate(percentage_area = total_area/sum(total_area)*100)
  
  ################################################################################
  ### Mangroves values
  
  ##Ecosystem services per square km at country scale → what are the countries selected for higher services?
  services_country <- PUs %>% 
    as_tibble %>%
    filter(Protected == 1) %>% 
    group_by(country) %>% 
    summarise(n_PUs = n(),
              Fishing = sum(Fishing_Intensity*AreaGMWKm), 
              Population = sum(POP*AreaGMWKm),
              Properties = sum(TOT_STOCK*AreaGMWKm),
              Carbon = sum(Tot_Carbon*AreaGMWKm),
              continent = first(continent)) %>% 
    left_join(total_country,  by = "country")
  
  country_stat <- services_country
  
  services_continent <- PUs %>% 
    as_tibble %>% 
    filter(Protected == 1) %>% 
    group_by(continent) %>% 
    summarise(n_PUs = n(),
              Fishing = sum(Fishing_Intensity*AreaGMWKm), 
              Population = sum(POP*AreaGMWKm),
              Properties = sum(TOT_STOCK*AreaGMWKm),
              Carbon = sum(Tot_Carbon*AreaGMWKm),
              continent = first(continent)) %>%  
    left_join(total_continent,  by = "continent")
  
  continent_stat <- services_continent
  
  ##Number of mangroves species by country?
  n_species_by_country <- PUs %>% 
    as_tibble %>% 
    dplyr::select(c(names(species), country)) %>% 
    group_by(country) %>% 
    summarise_all(sum) %>%
    mutate(nSpecies = rowSums(.!=0)) %>% 
    dplyr::select(country, nSpecies)
  
  country_stat <- country_stat %>% 
    left_join(n_species_by_country, by = 'country')
  
  n_species_by_continent <- PUs %>% 
    as_tibble %>% 
    dplyr::select(c(names(species), continent)) %>% 
    group_by(continent) %>% 
    summarise_all(sum) %>%
    mutate(nSpecies = rowSums(.!=0)) %>% 
    dplyr::select(continent, nSpecies)
  
  continent_stat <- continent_stat %>% 
    left_join(n_species_by_continent)
  
  ## Percentage of total mangroves protected by country
  protection_country <- PUs %>% 
    as_tibble() %>% 
    filter(Protected == "TRUE") %>% 
    group_by(country) %>% 
    summarise(tot_mangrove_protected = sum(AreaGMWKm))
  
  country_stat <- country_stat %>% 
    left_join(protection_country, by = 'country') %>% 
    mutate(prct_tot_mangrove_protected = tot_mangrove_protected/total_area*100)
  
  ## Percentage of total mangroves protected by continent
  protection_continent <- PUs %>% 
    as_tibble() %>% 
    filter(Protected == "TRUE") %>% 
    group_by(continent) %>% 
    summarise(tot_mangrove_protected = sum(AreaGMWKm))
  
  continent_stat <- continent_stat %>% 
    left_join(protection_continent, by = 'continent') %>% 
    mutate(prct_tot_mangrove_protected = tot_mangrove_protected/total_area*100)
  
  list(country_stat, continent_stat)
}