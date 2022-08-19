library(tidyverse)
library(sf)

fSelect_WDPA <- function(PUs) {
  cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

  pa_data <- readRDS("Data/clean_WDPA.rds")
  GMW <- st_read("Data/GMWValid/GMWValid.shp") %>% 
    st_transform(cCRS)
  
  pa_data <- pa_data %>% 
    filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV")) %>% 
    st_transform(cCRS) %>%
    st_make_valid() %>% 
    st_union() %>% 
    st_cast("POLYGON")
  
  #Maintain only GMW shapefiles that intersect with WDPA
  GMW_PA <- GMW %>% 
    st_intersects(pa_data)
  
  logi_int <- lengths(GMW_PA) > 0
  GMW <- GMW[logi_int == TRUE,]
  
  #Maintain only PUs that intersect with WDPA
  PUs_PA <- PUs %>% 
    st_intersects(pa_data)
  
  logi_int <- lengths(PUs_PA) > 0
  PUs_s <- PUs[logi_int == TRUE,]
  
  #Calculate the intersection between the mangrove of each PU with the protected areas
  PUs_GMW_WDPA <- PUs_s %>% 
    st_intersection(GMW) %>% 
    st_intersection(pa_data)
  
  #Calculate the area of the intersection
  PUs_GMW_WDPA$AreaWDPA <- PUs_GMW_WDPA %>% 
    st_area() %>%
    units::set_units(km^2)
  
  #Summarise the area covered by protected areas for each PU
  PUs_GMW_WDPA <- PUs_GMW_WDPA %>%
    as_tibble() %>%
    group_by(ID) %>%
    summarise(AreaWDPA = sum(AreaWDPA)) %>%
    mutate(AreaWDPA = ifelse(is.na(AreaWDPA), 0, AreaWDPA),
           AreaWDPA = units::set_units(AreaWDPA, km^2))
  
  #Join the calculated values to PUs
  PUs_s <- as_tibble(PUs_s) %>%
    left_join(PUs_GMW_WDPA, by="ID") %>%
    st_sf()
  
  #Areas with more than 50% of mangrove area covered by mangroves are defined as protected
  PUs_s$Protected <- ifelse(as.numeric(PUs_s$AreaWDPA) < as.numeric(PUs_s$AreaGMWKm)/2, "FALSE", "TRUE")
  
  PUs_protected <- PUs_s %>% 
    dplyr::select(c(ID, AreaWDPA, Protected)) %>% 
    st_drop_geometry()
  
  PUs <- as_tibble(PUs) %>%
    left_join(PUs_protected, by="ID") %>%
    st_sf() 
  
  #Replace the NA with FALSE
  PUs <- PUs %>% 
    mutate(across(Protected, ~ replace_na(., "FALSE"))) %>% 
    mutate(across(AreaWDPA,  ~ replace_na(., set_units(0, km^2))))
}

PUs <- PUs %>% 
  mutate(Protected = as.logical(Protected))
saveRDS(PUs, "RDS/PUs_Splitted_I_IV.rds")
