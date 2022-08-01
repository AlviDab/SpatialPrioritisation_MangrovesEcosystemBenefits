fIntersect_CoastalSqueeze <- function(PUs) {

  #Read the csv with the data
  temp <- list.files(path ="Data/CoastalSqueeze/Pop_20/", pattern = ".csv", all.files = TRUE, full.names = TRUE)
  rcp <- lapply (temp, function(x) {
    x %>% 
      read_csv %>%
      rename(CLSFID = ClsFID) %>% 
      rename_with(~paste0(., "_", str_extract(x, "rcp..")), .cols = !CLSFID)
  }
  )

  #Read the coastline files
  coastline_rcp <- st_read("Data/CoastalSqueeze/coastline_shp/DIVA_coastline.shp")
  
  #Join the csv with the coastline file
  rcp <- rcp %>% 
    lapply(function(x) {
      coastline_rcp %>% 
        left_join(x, by = "CLSFID") %>% 
        st_transform(cCRS) %>% 
        st_make_valid()
    }
    )
  
  #Check that PUs are arranged by ID
  PUs <- PUs %>% 
    arrange(ID)
  
  #Intersect with the PUs and calculate the mean value of absolute and percent change for each PU
  mean_squeeze_rcp <- lapply(rcp, function(x) {
    st_intersects(PUs, x) %>% 
      lapply(function(y) {
        x %>% 
          st_drop_geometry() %>% 
          tibble() %>% 
          slice(y) %>% 
          dplyr::select(starts_with(c("Absolute_change", "Percent_change"))) %>% 
          summarise(across(everything(), mean, na.rm = TRUE))
      }
      )
  }
  )

  detach("package:dplyr", unload = TRUE)
  library(plyr)

  #Create a dataframe with the mean squeeze in each element of the list
  mean_squeeze_rcp <- lapply(mean_squeeze_rcp, ldply) 
  
  detach("package:plyr", unload = TRUE)
  library(dplyr)
  
  #Add an ID column for each dataframe
  mean_squeeze_rcp <- lapply(mean_squeeze_rcp, function(x) {
    x %>%
      mutate(ID = as.numeric(row.names(.)))
  }
  )
  
  #Left_join each result with PUs
  for(i in 1:length(mean_squeeze_rcp)) {
    PUs <- left_join(PUs, mean_squeeze_rcp[[i]], by = "ID")
  }
  
  #Use NN to fill NA values
  source("Functions/fRemove_NANearestNeighbourg.R")
  
  PUs <- fNN_x(PUs, Percent_change_rcp26) 
  PUs <- fNN_x(PUs, Absolute_change_rcp26)
  PUs <- fNN_x(PUs, Percent_change_rcp45) 
  PUs <- fNN_x(PUs, Absolute_change_rcp45)
  PUs <- fNN_x(PUs, Percent_change_rcp85) 
  PUs <- fNN_x(PUs, Absolute_change_rcp85)
}

