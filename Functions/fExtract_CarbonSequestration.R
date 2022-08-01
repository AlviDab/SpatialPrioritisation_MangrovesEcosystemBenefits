Extract_Carbon <- function(x) {

library(future.apply)
library(exactextractr)

  s <- list.files(path = x, pattern='tif$', full.names=TRUE) %>%
  future_lapply(rast) #I produce the raster of the list of file of carbon storage
  
PUs <- PUs %>%
  st_transform(crs = 4326) %>% #I transform the crs of the PUs so that have the same crs of the rasters
  st_make_valid()

#Function to extract the carbon data for each PUs
carb_data_list <- future_lapply(s, function(x) {
                           a <- exact_extract(x, PUs, c('mean', 'count')) #%>% #I extract all the values that intercept with the PUs
                           }
                         )

a <- carb_data_list %>% 
  future_lapply(function(x) {
    x %>% 
      mutate(ID = PUs$ID) %>% 
      filter(mean > 0)
    })

library(data.table)

final_carbon <- a %>%
  rbindlist() %>% 
  as_tibble() %>% 
  group_by(ID) %>%
  summarise(carbon = weighted.mean(mean, count)) %>%
  mutate(ID = as.numeric(ID))

PUs <- left_join(PUs, final_carbon, by = "ID") %>% 
  st_transform(crs = cCRS) #I transform the crs of the PUs

return(PUs)
}