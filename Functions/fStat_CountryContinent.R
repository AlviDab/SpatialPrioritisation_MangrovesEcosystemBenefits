################################################################################
###Statistics by country and region
##Priority areas

#n is the rank
source("Functions/fSelect_PriorityAreasProtected.r")

fStat_CountryContinent <- function(result, n) { ##Percentage of mangroves in priority areas (top 10%)
  #Total mangroves by country
  total_country <- PUs %>% 
    as_tibble() %>% 
    group_by(country) %>% 
    summarise(total_area = sum(AreaGMWKm))
  
  ## Percentage of top-x priority areas by country
  priority_area_cover_country <- result %>% 
    as_tibble() %>% 
    filter(rank <= n) %>% 
    group_by(country) %>% 
    summarise(priority_area_cover = sum(AreaGMWKm)) %>% 
    left_join(total_country,  by = "country") %>% 
    mutate(perc_priority_area_country = priority_area_cover/total_area*100,
           total_perc_priority_area_country = priority_area_cover/sum(priority_area_cover)*100)
  
  country_stat <- priority_area_cover_country
  
  ## by continent
  
  # Area by continent
  total_continent <- PUs %>% 
    as_tibble() %>% 
    group_by(continent) %>% 
    summarise(total_area = sum(AreaGMWKm)) %>%
    mutate(percentage_area = total_area/sum(total_area)*100)
  
  # priority areas by continent
  priority_area_cover_continent <- result %>% 
    as_tibble() %>% 
    filter(rank <= n) %>% 
    group_by(continent) %>% 
    summarise(priority_area_cover = sum(AreaGMWKm)) %>% 
    left_join(total_continent,  by = "continent") %>% 
    mutate(perc_priority_area_continent = priority_area_cover/total_area*100,
           total_perc_priority_area_continent = priority_area_cover/sum(priority_area_cover)*100)
  
  continent_stat <- priority_area_cover_continent
  
  ################################################################################
  ### Mangroves values
  
  ##Ecosystem services per square km at country scale → what are the countries selected for higher services?
  services_country <- result %>% 
    as_tibble %>% 
    filter(rank <= n) %>% 
    group_by(country) %>% 
    summarise(n_PUs = n(),
              Fishing = sum(Fishing_Intensity*AreaGMWKm), 
              Population = sum(POP*AreaGMWKm),
              Properties = sum(TOT_STOCK*AreaGMWKm),
              Carbon = sum(Tot_Carbon*AreaGMWKm),
              Fishing_km2 = sum(Fishing_Intensity)/n_PUs, 
              Population_km2 = sum(POP)/n_PUs,
              Properties_km2 = sum(TOT_STOCK)/n_PUs,
              Carbon_km2 = sum(Tot_Carbon)/n_PUs,
              continent = first(continent))
  
  country_stat <- country_stat %>% 
    left_join(services_country, by = 'country')
  
  services_continent <- result %>% 
    as_tibble %>% 
    filter(rank <= n) %>% 
    group_by(continent) %>% 
    summarise(n_PUs = n(),
              Fishing = sum(Fishing_Intensity*AreaGMWKm), 
              Population = sum(POP*AreaGMWKm),
              Properties = sum(TOT_STOCK*AreaGMWKm),
              Carbon = sum(Tot_Carbon*AreaGMWKm),
              Fishing_km2 = sum(Fishing_Intensity)/n_PUs, 
              Population_km2 = sum(POP)/n_PUs,
              Properties_km2 = sum(TOT_STOCK)/n_PUs,
              Carbon_km2 = sum(Tot_Carbon)/n_PUs,
              continent = first(continent))
  
  continent_stat <- continent_stat %>% 
    left_join(services_continent)
  
  ##Number of mangroves species by country?
  n_species_by_country <- PUs_NotSplitted %>% 
    as_tibble %>% 
    dplyr::select(c(names(species), country)) %>% 
    group_by(country) %>% 
    summarise_all(sum) %>%
    mutate(nSpecies = rowSums(.!=0)) %>% 
    dplyr::select(country, nSpecies)
  
  country_stat <- country_stat %>% 
    left_join(n_species_by_country, by = 'country')
  
  n_species_by_continent <- PUs_NotSplitted %>% 
    as_tibble %>% 
    dplyr::select(c(names(species), continent)) %>% 
    group_by(continent) %>% 
    summarise_all(sum) %>%
    mutate(nSpecies = rowSums(.!=0)) %>% 
    dplyr::select(continent, nSpecies)
  
  continent_stat <- continent_stat %>% 
    left_join(n_species_by_continent)
  
  ## Percentage of total mangroves protected by country/continent
  protection_country <- PUs %>% 
    as_tibble() %>% 
    filter(Protected == "TRUE") %>% 
    group_by(country) %>% 
    summarise(tot_mangrove_protected = sum(AreaGMWKm))
  
  country_stat <- country_stat %>% 
    left_join(protection_country, by = 'country') %>% 
    mutate(prct_tot_mangrove_protected = tot_mangrove_protected/total_area)
  
  ## Percentage of total mangroves protected by continent/continent
  protection_continent <- PUs %>% 
    as_tibble() %>% 
    filter(Protected == "TRUE") %>% 
    group_by(continent) %>% 
    summarise(tot_mangrove_protected = sum(AreaGMWKm))
  
  continent_stat <- continent_stat %>% 
    left_join(protection_continent, by = 'continent') %>% 
    mutate(prct_tot_mangrove_protected = tot_mangrove_protected/total_area)
  
  ################################################################################
  ### Comparison priority protected and non protected areas
  ## Percentage priority areas protected
  
  priority_areas_protected <- f_PriorityAreasProtected(result, n)
  
  # By country
  country_stat <- country_stat %>%
    left_join(priority_areas_protected[[1]], by = 'country')
  
  # By region
  continent_stat <- continent_stat %>%
    left_join(priority_areas_protected[[2]], by = 'continent')
  
  list(country_stat, continent_stat)
}
