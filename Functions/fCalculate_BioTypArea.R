# Author: Alvise Dabalà
# Calculate the area of biophysical typology in each planning unit
# Does not require any variable in input

fCalc_BioTypArea <- function() {
  
  Classes <- c(unique(BioTyp$Class), unique(BioTyp$Sedimentar))
  
  Calc_AreaBioTyp <- function(x) {
    BioTyp <- BioTyp %>% 
      filter(Class == x | Sedimentar == x)
    
    PUsInt <- st_intersection(PUs, BioTyp)#Make an intersection between the PUs and GMW data
    
    #Calculate the area of each polygon of the intersection in squared km
    
    PUsInt$AreaBioTyp <- st_area(PUsInt) %>% #Calculate the area
      units::set_units(km^2) #Set the unit
    
    #Create a vector with the area of GMW for each PU and add it to PUs
    
    AreaPUs <- PUsInt %>% 
      as_tibble() %>% 
      dplyr::select(-geometry) %>% 
      group_by(ID) %>% 
      summarise(AreaBioTyp = sum(AreaBioTyp)) %>% #Calculate the total area of GMW for each PU
      dplyr::select(ID, AreaBioTyp) #Select only ID and AreaGMW columns
    
    PUs <- PUs %>%
      as_tibble() %>% #Transform to tibble
      left_join(AreaPUs, by = "ID") %>% #Add the column AreaGMW to PUs by "ID"
      mutate(AreaBioTyp = ifelse(is.na(AreaBioTyp), 0, AreaBioTyp),
             AreaBioTyp = units::set_units(AreaBioTyp, km^2)) %>% 
      dplyr::select(ID, AreaBioTyp) #Select only ID and AreaGMW columns

    names(PUs)[names(PUs) == 'AreaBioTyp'] <- x
    
    PUs
    }
  
  Areas_BioTyp <- lapply(Classes, Calc_AreaBioTyp)
  
  library(plyr)
  Areas_BioTyp <- join_all(Areas_BioTyp, by='ID', type='left')
  
  detach("package:plyr", unload=TRUE) #I unload the package because cause problem with dplyr
  library(dplyr)
  
  PUs <- PUs %>% 
    left_join(Areas_BioTyp, by = "ID")
}