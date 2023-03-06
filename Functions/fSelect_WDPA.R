library(tidyverse)
library(sf)
library(units)

fSelect_WDPA <- function(PUs) {
  cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

  pa_data <- readRDS("RDS/WDPA_polygon_points_123_clean.rds")
  GMW <- st_read("Data/GMWValid/GMWValid.shp") %>% 
    st_transform(cCRS)
  
  pa_data <- pa_data %>% 
    filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV")) %>% 
    st_transform(cCRS) %>%
    st_make_valid() %>% 
    st_union() %>% 
    st_cast("POLYGON")
  
  saveRDS(pa_data, "RDS/pa_data_union_I_IV.rds")
  
  #Maintain only GMW shapefiles that intersect with WDPA
  GMW_PA <- GMW %>% 
    st_intersects(pa_data)
  
  logi_int <- lengths(GMW_PA) > 0
  GMW <- GMW[logi_int == TRUE,]
  
  saveRDS(GMW_PA, "RDS/GMW_PA_I_IV.rds")
  
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
  
  #WDPA Fiji (PUs 9111)
  wdpa_FJI <- readRDS("RDS/wdpa_FJI.rds")
  
  PUs_9111 <- PUs[9111,]
  
  #Open GMW
  GMW <- readRDS("RDS/GMW_PUs_9111.rds")
  
  wdpa_FJI <- wdpa_FJI %>% 
    st_transform(crs = cCRS)
  
  wdpa_FJI_PUs_9111 <- wdpa_FJI %>% 
    st_intersects(PUs_9111)
  
  logi_int <- lengths(wdpa_FJI_PUs_9111) > 0
  wdpa_FJI <- wdpa_FJI[logi_int == TRUE,] #no intersections so I do not save the file
  
  #Make the union of the PAs
  wdpa_FJI <- wdpa_FJI %>%
    filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV")) %>% 
    st_transform(cCRS) %>%
    st_make_valid() %>% 
    st_union() %>% 
    st_cast("POLYGON")
  
  GMW_PUs_9111 <- GMW %>% 
    st_intersects(PUs_9111)
  
  logi_int <- lengths(GMW_PUs_9111) > 0
  GMW <- GMW[logi_int == TRUE,]
  
  saveRDS(GMW, "RDS/GMW_PUs_9111.rds")
  
  #Calculate the intersection between the mangrove of each PU with the protected areas
  PUs9111_GMW_WDPA <- PUs_9111 %>% 
    st_intersection(GMW) %>% 
    st_intersection(wdpa_FJI)
  
  PUs[9111, "AreaWDPA"] <- 0 #There is no intersection, so the area is 0
  
  #Areas with more than 50% of mangrove area covered by mangroves are defined as protected
  PUs$Protected <- ifelse(as.numeric(PUs$AreaWDPA) < as.numeric(PUs$AreaGMWKm)/2, "FALSE", "TRUE")
}

PUs <- PUs %>% 
  mutate(Protected = as.logical(Protected))
saveRDS(PUs, "RDS/PUs_Splitted_I_IV_and_All_9111.rds")
