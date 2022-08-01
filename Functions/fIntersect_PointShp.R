#Alvise Dabalà
#2022-03-21

#Function to intersect PUs with points values

#INPUT
#PUs = shapefile
#Point_shp = shapefile
#col_name = vector containg the column name

fIntersect_PointShp <- function(PUs, Point_shp, col_name) {
  #Arrange Pus by ID
  PUs <- PUs %>% 
    arrange(ID)
  
  #Project to the crs of the PUs
  Point_shp <- Point_shp %>% 
    st_transform(crs = cCRS) %>% 
    st_make_valid()
  
  PUs_Int <- st_intersects(PUs, Point_shp) %>% 
    lapply(function(x) {
      Point_shp %>%
        st_drop_geometry() %>% 
        tibble() %>% 
        slice(x) %>% 
        summarise_at(col_name, mean, na.rm = TRUE)
    }
    )
 
  PUs_Int <- tibble(do.call(rbind, PUs_Int))
  
  PUs <- PUs %>% 
    add_column(PUs_Int)
}

