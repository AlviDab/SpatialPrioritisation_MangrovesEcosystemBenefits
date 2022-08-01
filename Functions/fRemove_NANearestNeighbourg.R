# 10-03-2022
## Function to subsitute NAs values in a column with the value of the nearest PU

fNN_x <- function(x, colName) {
  x <- x %>% 
    arrange(ID)
  
  colName <- enquo(colName)
  
  Value <-  x %>% 
    filter(!is.na(!!colName)) #NA rows
  
  NoValue <- x %>% 
    filter(is.na(!!colName)) #rows with values
  
  NN_Value <- Value %>% 
    slice(as_vector(st_nearest_feature(NoValue, Value))) %>% 
    st_drop_geometry() %>% 
    dplyr::select(!!colName) %>% 
    as_vector()
  
  NoValue <- NoValue %>%
    mutate(!!colName := NN_Value)

  x <- bind_rows(Value, NoValue) %>%
    arrange(ID)
}
