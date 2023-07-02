library(tidyverse)
library(sf)
library(units)

fSelect_AllWDPA <- function(PUs) {
  cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

  pa_data <- readRDS("RDS/WDPA_polygon_points_123_clean.rds")
  
  GMW <- st_read("Data/GMWValid/GMWValid.shp") %>% 
    st_transform(cCRS)
  
  pa_data <- pa_data %>% 
    st_transform(cCRS) %>%
    st_make_valid() %>% 
    st_union() %>% 
    st_cast("POLYGON")
  
  saveRDS(pa_data, "RDS/pa_data_IUCN_I_VI.rds")
  
  pa_data <- readRDS("RDS/pa_data_IUCN_I_VI.rds")
  
  #Maintain only GMW shapefiles that intersect with WDPA
  GMW_PA <- GMW %>% 
    st_intersects(pa_data)
  
  logi_int <- lengths(GMW_PA) > 0
  GMW <- GMW[logi_int == TRUE,]
  
  saveRDS(GMW, "RDS/GMW_IUCN_I_VI.rds")
  
  #Maintain only PUs that intersect with WDPA
  PUs_PA <- PUs %>% 
    st_intersects(pa_data)
  
  logi_int <- lengths(PUs_PA) > 0
  PUs_s <- PUs[logi_int == TRUE,]
  
  #Calculate the intersection between the mangrove of each PU with the protected areas
  PUs_GMW_WDPA <- PUs_s %>% 
    st_intersection(GMW) %>% 
    st_intersection(pa_data)
  
  saveRDS(PUs_GMW_WDPA, "RDS/PUs_GMW_WDPA.rds")
  
  #Calculate the area of the intersection
  PUs_GMW_WDPA$AreaWDPA_I_VI <- PUs_GMW_WDPA %>% 
    st_area() %>%
    units::set_units(km^2)
  
  saveRDS(PUs_GMW_WDPA, "RDS/PUs_GMW_WDPA.rds")
  
  #Summarise the area covered by protected areas for each PU
  PUs_GMW_WDPA <- PUs_GMW_WDPA %>%
    as_tibble() %>%
    group_by(ID) %>%
    summarise(AreaWDPA_I_VI = sum(AreaWDPA_I_VI)) %>%
    mutate(AreaWDPA_I_VI = ifelse(is.na(AreaWDPA_I_VI), 0, AreaWDPA_I_VI),
           AreaWDPA_I_VI = units::set_units(AreaWDPA_I_VI, km^2))
  
  saveRDS(PUs_GMW_WDPA, "RDS/PUs_GMW_WDPA.rds")
  
  #Join the calculated values to PUs
  PUs_s <- as_tibble(PUs_s) %>%
    left_join(PUs_GMW_WDPA, by="ID") %>%
    st_sf()
  
  #Areas with more than 50% of mangrove area covered by mangroves are defined as protected
  PUs_s$Protected_I_VI <- ifelse(as.numeric(PUs_s$AreaWDPA_I_VI) < as.numeric(PUs_s$AreaGMWKm)/2, "FALSE", "TRUE")
  
  PUs_protected <- PUs_s %>% 
    dplyr::select(c(ID, AreaWDPA_I_VI, Protected_I_VI)) %>% 
    st_drop_geometry()
  
  PUs <- as_tibble(PUs) %>%
    left_join(PUs_protected, by="ID") %>%
    st_sf() 
  
  #Replace the NA with FALSE
  PUs <- PUs %>% 
    mutate(across(Protected_I_VI, ~ replace_na(., "FALSE"))) %>% 
    mutate(across(AreaWDPA_I_VI, ~ replace_na(., set_units(0, km^2))))

  PUs <- PUs %>% 
    mutate(Protected_I_VI = as.logical(Protected_I_VI))
  
  saveRDS(PUs, "RDS/PUs_Splitted_Protection_I_VI_9119.rds")
  
  return(PUs)
}
