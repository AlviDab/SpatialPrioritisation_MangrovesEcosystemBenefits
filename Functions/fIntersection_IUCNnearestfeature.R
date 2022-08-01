fIntersection_IUCNnearestfeature <- function(PUs, IUCN) {
 
  #I project the PUs
  PUs <- PUs %>%
    st_transform(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  
  #Intersection and selection of the variables of interest
  PUs <- st_join(PUs, IUCN, join = st_intersects) %>%
    dplyr::select(c(colnames(PUs), binomial))
    
  PUs$AreaGMWKm <- as.numeric(PUs$AreaGMWKm) %>% 
    replace(as.numeric(PUs$AreaGMWKm) < 1e-6, 0) #Replace with zero values of area that are too small
  PUs$AreaGMWKm1 <- PUs$AreaGMWKm

  PUs <- PUs %>% 
    as_tibble() %>% #Transform in a tibble
    pivot_wider(names_from = binomial, values_from = AreaGMWKm1, names_prefix = "Bin_", values_fill = 0) %>% #I make the tibble wide using binomial and filling the missing data of binomial with zeros and I add a prefix for the binomial vaiables
    st_sf() #Transform in a sf
    
  #Production of PUs without species information
  PUsNoInfo <- PUs %>%
    dplyr::filter(`Bin_NA` != 0) #Select all the PUs that do not intersect with IUCN - Redlist (Bin_NA = area that do not intersect)
  
  #Production of PUs with species information
  PUsInfo <- PUs %>%
    dplyr::filter(`Bin_NA` == 0) #Select all the PUs that intersect with IUCN - Redlist
  
  ClosestPUs <- st_nearest_feature(PUsNoInfo, PUsInfo) #Produce a list with the number of row of the nearest PUsInfo to PUsNoInfo
  
  # c <- PUsNoInfo %>% #Select row i of PUsNoInfo
  #   as_tibble() %>% #Transform to tibble
  #   dplyr::select(`Bin_NA`) #Select the variable Bin_NA of those columns
      
  AreaValues <- PUsInfo[ClosestPUs,] %>% #Select the row a of PUsInfo
    as_tibble() %>% #Transform in tibble
    dplyr::select(starts_with("Bin_")) %>% #Select all the column that start with Bin_
    mutate_at(vars(everything()),
                list(~ ifelse( . == 0, 0, PUsNoInfo$`Bin_NA`))) #I mutate all the columns (that start with Bin_) to the value of Bin_NA if they are not equal to zero, if not to zero
    
  PUsNoInfo <- PUsNoInfo %>% #Select row i of PUsNoInfo
    as_tibble() %>% #Transform to tibble
    dplyr::select(-starts_with("Bin_")) %>% #Remove all the variable that start with Bin_
    add_column(AreaValues) %>% #Add all the column of d
    st_sf() #Produce a shapefile
  
  PUs <- rbind(PUsInfo, PUsNoInfo) %>% #I bind PUsInfo and PUsNoInfo
    dplyr::select(-"Bin_NA") #I remove the column Bin_NA
  
  #Select only the columns of mangroves species
  PUs_sp <- PUs %>% 
    dplyr::select(starts_with("Bin_")) %>%
    st_drop_geometry() %>%
    rename_all(~stringr::str_replace(.,"^Bin_",""))
  
  #Calculate the number of PUs that intercept with each species distribution
  nPUs <- (colSums(PUs_sp != 0)) %>%
    bind_rows() %>%
    pivot_longer(everything()) %>%
    rename(binomial = name)
  
  #Number of species in each PU
  PUs_Species <- PUs_sp %>% 
    mutate(tot = rowSums(PUs_sp != 0)) %>% 
    dplyr::select(tot) %>% 
    mutate(geometry = PUs$geometry) %>% 
    st_sf()
  
  #Remove the Bin_ before the name of the species and arrange by species
  PUs <- PUs %>%  
    rename_all(~stringr::str_replace(.,"^Bin_","")) %>% 
    arrange(ID)
  
  #List that will be returned by the function
  Result <- list(PUs, nPUs, PUs_Species)
  
  return(Result)
}