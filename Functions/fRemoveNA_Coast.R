# 10-03-2022
## Function to subsitute NAs values in a column with the value of the nearest PU

fNN_Coast_x <- function(PUs) {
  PUs <- PUs %>% 
    arrange(ID)
  
  Value <-  PUs %>% 
    filter(!is.na(TOT_STOCK)) %>%  #NA rows
    mutate(IDkm2 = row.names(.))
  
  NoValue <- PUs %>% 
    filter(is.na(TOT_STOCK)) #rows with values
  
  Coast_km2 <- NoValue %>% 
    mutate(IDkm2 = as_vector(st_nearest_feature(NoValue, Value))) %>% 
    rbind(Value) %>% 
    group_by(IDkm2) %>% 
    mutate(AreaGMWTotkm = sum(AreaGMWKm),
           TOT_STOCK = TOT_STOCK/as.numeric(AreaGMWTotkm),
           POP = POP/as.numeric(AreaGMWTotkm)) %>% 
    dplyr::select(!IDkm2 & !AreaGMWTotkm)
  
  Value <-  Coast_km2 %>% 
    filter(!is.na(TOT_STOCK))  #NA rows
  
  NoValue <- Coast_km2 %>% 
    filter(is.na(TOT_STOCK))
  
  NN_Value <- Value %>% 
    slice(as_vector(st_nearest_feature(NoValue, Value))) %>% 
    st_drop_geometry() %>% 
    dplyr::select(TOT_STOCK, POP)
  
  NoValue <- NoValue %>%
    mutate(TOT_STOCK = NN_Value$TOT_STOCK,
           POP = NN_Value$POP)
  
  PUs <- bind_rows(Value, NoValue) %>%
    arrange(ID)
}
