# Alvise Dabalà <2022-03-07>

# Updated: 2022-03-15

# Fuction to calculate the area of mangroves already protected for each PU 
# PUs with more than 50% of mangroves covered by PA are considered protected

# Inputs
# PUs = planning units shapefile (Robinson projection)
# GMW = global distribution of mangroves shapefile (Robinson projection)
library(tidyverse)
library(sf)

cCRS <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

PUs <- readRDS("Data/GMW_MangrovePUsArea.rds")
GMWtxt <- "Data/GMWValid/GMWValid.shp"
path_txt <- "Data/WDPA/WDPA_poly"
  
GMW <- GMWtxt %>%
  st_read() %>% #Read the GMW shapefile
  st_transform(crs = cCRS) #Project GMW to degrees to make the cut

fIntersection_MPAs <- function(PUs, GMW) {
  library(wdpar)
  library(dplyr)
  library(ggmap)
  
  cCRS <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  #Load the data
  list_data <- list.files(path = path_txt, pattern='shp$', full.names=TRUE) 
  
  PUs <- PUs %>% 
    st_transform(st_crs(raw_pa_data))
  
  lapply(list_data[2:3], function(x) {
  raw_pa_data <-  list_data[[3]] %>% 
    st_read() %>% 
    dplyr::filter(IUCN_CAT != "V") %>% 
    dplyr::filter(IUCN_CAT != "VI")
  
  a <- st_is_valid(raw_pa_data)
  
  b <- raw_pa_data[c(56540,
                     56541,
                     56542,
                     56543),] %>% 
    st_repair_geometry(geometry_precision = 1500)
  
  raw_pa_data <- raw_pa_data[-c(56540,
                                56541,
                                56542,
                                56543), ] %>% 
    rbind(b)
  
  PA_PUs <- raw_pa_data %>% 
    st_intersects(PUs)
  
  logi_int <- lengths(PA_PUs) > 0
  raw_pa_data <- raw_pa_data[logi_int == TRUE,]
  
  x <- gsub(".shp$", ".rds", x)
  
  saveRDS(raw_pa_data, "WDPA_PUs3.rds")
  }
  )
  
  WDPA_PUs <- WDPA_PUs1 %>% 
    rbind(WDPA_PUs2) %>% 
    rbind(WDPA_PUs3)
  
  st_write(WDPA_PUs, "WDPA_PUs.shp")
  
  raw_pa_data <- wdpa_fetch("global", wait = TRUE,
                                download_dir = rappdirs::user_data_dir("wdpar")) %>% 
    filter(IUCN_CAT != "V" | IUCN_CAT != "VI") #Removing IUCN category V and VI
  
  #Clean the data
  pa_data <- wdpa_clean(WDPA_PUs) #Cleaning the data
  
  #Dissolve the data
  dissolved_pa_data <- wdpa_dissolve(pa_data) %>% 
    st_transform(crs = cCRS) %>% 
    st_make_valid()
  
  #Intersect the mangrove of each PU with the protected areas
  PUs_GMW_WDPA <- st_intersection(PUs, GMW) %>% 
    st_intersection(dissolved_pa_data)

  #Calculate the area of the intersection
  PUs_GMW_WDPA$AreaWDPA <- st_area(PUs_GMW_WDPA) %>%
    units::set_units(km^2)

  #Summarise the area covered by protected areas for each PU
  PUs_GMW_WDPA <- PUs_GMW_WDPA %>%
    as_tibble() %>%
    group_by(ID) %>%
    summarise(AreaWDPA = sum(AreaWDPA)) %>%
    mutate(AreaWDPA = ifelse(is.na(AreaWDPA), 0, AreaWDPA),
           AreaWDPA = units::set_units(AreaWDPA, km^2))

  #Join the calculated values to PUs and change NAs with zero values
  PUs <- as_tibble(PUs) %>%
    left_join(PUs_GMW_WDPA, by="ID") %>%
    st_sf()

  #Areas with more than 50% of mangrove area covered by mangroves are defined as protected
  PUs$Protected <- ifelse(PUs$AreaWDPA < PUs$AreaGMWKm/2, "FALSE", "TRUE")

  return(PUs)

}

