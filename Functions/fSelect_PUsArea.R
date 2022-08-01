fSelect_PUsArea <- function(PUs, GMW) {
  
  PUsInt <- st_intersection(PUs, GMW)#Make an intersection between the PUs and GMW data
  
  rm(GMW) #I don't need it after so I free space
  
  #Calculate the area of each polygon of the intersection in squared km
  PUsInt$AreaGMWKm <- st_area(PUsInt) %>% #Calculate the area
    units::set_units(km^2) #Set the unit
  
  #Create a vector with the area of GMW for each PU and add it to PUs
  AreaPUs <- PUsInt %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    group_by(ID) %>% 
    summarise(AreaGMWKm = sum(AreaGMWKm)) %>% #Calculate the total area of GMW for each PU
    dplyr::select(ID, AreaGMWKm) #Select only ID and AreaGMW columns
  
  PUs <- PUs %>%
    as_tibble() %>% #Transform to tibble
    left_join(AreaPUs, by = "ID") %>% #Add the column AreaGMW to PUs by "ID"
    mutate(AreaGMWKm = ifelse(is.na(AreaGMWKm), 0, AreaGMWKm),
           AreaGMWKm = units::set_units(AreaGMWKm, km^2)) %>% 
    st_sf() #Transform to shapefile
  
  #Area <- 400 %>% #Area of the PUs
    #units::set_units(km^2) #Set the unit
  
  #Define include = FALSE if the area of GMW in the PU is less than i of the area of a PU
  #PUs$Include <- ifelse(PUs$AreaGMWKm < Area * i/100, "FALSE", "TRUE")
  
  #Remove all the PUs that are not included and plot the resulting PUs
  #PUsIncluded <- filter(PUs, Include == "TRUE")
  
  return(PUs)
}
