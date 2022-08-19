#Author: Jason Everett

fSpatPlan_Get_Boundary <- function(Limits, cCRS){
  
  # Create function for creating polygon
  polygon <- function(x){
    x <- x %>% 
      as.matrix() %>%
      list() %>%
      st_polygon() %>%
      st_sfc(crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
      st_transform(crs = cCRS)
  }
  
  # if Pacific-centered, two polygons then union them:
  if (is.numeric(Limits) && cCRS == "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){
    
    Bndry1 <- tibble(x = seq(-180, Limits["xmin"], by = 1), y = Limits["ymin"]) %>%
      bind_rows(tibble(x = Limits["xmin"], y = seq(Limits["ymin"], Limits["ymax"], by = 1))) %>%
      bind_rows(tibble(x = seq(Limits["xmin"], -180, by = -1), y = Limits["ymax"])) %>%
      bind_rows(tibble(x = -180, y = seq(Limits["ymax"], Limits["ymin"], by = -1))) %>% 
      polygon()
    
    Bndry2 <- tibble(x = seq(180, Limits["xmax"], by = -1), y = Limits["ymin"]) %>%
      bind_rows(tibble(x = Limits["xmax"], y = seq(Limits["ymin"], Limits["ymax"], by = 1))) %>%
      bind_rows(tibble(x = seq(Limits["xmax"], 180, by = 1), y = Limits["ymax"])) %>%
      bind_rows(tibble(x = 180, y = seq(Limits["ymax"], Limits["ymin"], by = -1))) %>% 
      polygon()
    
    Bndry <- st_union(Bndry1, Bndry2)

    return(Bndry)
  }
  
  if (is.numeric(Limits)){
    Bndry <- tibble(x = seq(Limits["xmin"], Limits["xmax"], by = 1), y = Limits["ymin"]) %>%
      bind_rows(tibble(x = Limits["xmax"], y = seq(Limits["ymin"], Limits["ymax"], by = 1))) %>%
      bind_rows(tibble(x = seq(Limits["xmax"], Limits["xmin"], by = -1), y = Limits["ymax"])) %>%
      bind_rows(tibble(x = Limits["xmin"], y = seq(Limits["ymax"], Limits["ymin"], by = -1))) %>%
      polygon()
    
    return(Bndry)
  }
  
  if (Limits == "Global"){
    Bndry <- tibble(x = seq(-180, 180, by = 1), y = -90) %>%
      bind_rows(tibble(x = 180, y = seq(-90, 90, by = 1))) %>%
      bind_rows(tibble(x = seq(180, -180, by = -1), y = 90)) %>%
      bind_rows(tibble(x = -180, y = seq(90, -90, by = -1))) %>%
      polygon()
    
    return(Bndry)
  }
  
  # Added Western Pacific
  if (Limits == "WestPacific"){
    source("SpatPlan_Extras.R")
    source("fSpatPlan_Convert2PacificRobinson.R")
    source("fSpatPlan_Get_MaskedPolygon.R")
    
    ocean_sf <- ne_download(scale = "large", category = "physical", type = "geography_marine_polys", returnclass = "sf") 
    
    # Check the seas listed in ocean_sf
    # list <- unique(ocean_sf$label) %>% 
    #  as_tibble()
    # list[order(list$value),] %>% print(n = Inf) # print everything
    
    # Create the list of water bodies to be included
    ocean_list <- c("NORTH PACIFIC OCEAN", "SOUTH PACIFIC OCEAN", "Philippine Sea", 
                    "Coral Sea", "Tasman Sea", "Bay of Plenty", "Cook Strait", 
                    "Bismarck Sea", "Solomon Sea", "Bass Strait", "Gulf of Papua", 
                    "Banda Sea", "Java Sea", "Celebes Sea", "Makassar Strait",
                    "Molucca Sea", "Halmahera Sea", "Ceram Sea", "Flores Sea")
    # Filter out ocean_list from ocean_sf
    ocean_temp <- ocean_sf %>% 
      dplyr::filter(label %in% ocean_list) %>% 
      dplyr::select(label, ne_id, geometry)
    
    projected_ocean <- fSpatPlan_Convert2PacificRobinson(ocean_temp) # Project it to Pacific Robinson
    
    # Call and filter the EEZs
    eez <- st_read("Data/World_EEZ_v11/eez_v11.shp") %>% 
      filter(SOVEREIGN1 != "Antarctica") %>% 
      dplyr::select(MRGID, GEONAME, TERRITORY1, X_1, Y_1, geometry)
    
    # Create the list of EEZs
    eez_list <- c("Phoenix Group", "Papua New Guinea", "Guam",
                  "Micronesia", "Nauru", "Solomon Islands", "Vanuatu", "New Caledonia", "Fiji",
                  "Marshall Islands", "Tonga", "Niue", "American Samoa", "Samoa",
                  "Tokelau", "Cook Islands", "French Polynesia", "Northern Mariana Islands", "Gilbert Islands",
                  "Tuvalu", "Wallis and Futuna", "Line Group", "Palau", "Indonesia")
    eez_filtered <- eez %>% 
      dplyr::filter(TERRITORY1 %in% eez_list)
    
    # Create polygons that will be used to include ABNJ (+: east/north; -: west/south)
    
    polygon_crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    polygon1 <- fSpatPlan_Get_Boundary(c(xmin = 155, xmax = 176, ymax = 5, ymin = -10), polygon_crs) %>% 
      st_as_sf() %>% 
      st_make_valid()
    polygon2 <- fSpatPlan_Get_Boundary(c(xmin = -161, xmax = -153, ymax = -10, ymin = -19), polygon_crs) %>% 
      st_as_sf() %>% 
      st_make_valid()
    polygon3 <- fSpatPlan_Get_Boundary(c(xmin = 135, xmax = 152, ymax = 6, ymin = 0), polygon_crs) %>% 
      st_as_sf() %>% 
      st_make_valid()
    polygon4 <- fSpatPlan_Get_Boundary(c(xmin = 170, xmax = 176, ymax = -10, ymin = -20), polygon_crs) %>% 
      st_as_sf() %>% 
      st_make_valid()
    
    # Include areas in ocean_temp that are within eez_temp & merge ABNJ
    # Note, you might want to increase memory limit here
    # memory.limit(size = 56000) # for Windows
    merged <- fSpatPlan_Get_MaskedPolygon(projected_ocean, 0.25, eez_filtered, FALSE)
    merged <- merged %>%  # change to inverse = TRUE, if you want the inverse of the intersection of ocean_temp and eez_temp
      st_union(., polygon1) %>% # merge with all the individual polygons
      st_union(., polygon2) %>% 
      st_union(., polygon3) %>% 
      st_union(., polygon4)
    
    # Filter out land masses found within the boundaries of the EEZs of interest
    land_list <- c("Palau", "Guam", "Indonesia", "Northern Mariana Islands", "Federated States of Micronesia", "Papua New Guinea",
                   "Solomon Islands", "New Caledonia", "Vanuatu", "Fiji", "Marshall Islands", "Nauru", "Kiribati", "Tuvalu",
                   "Wallis and Futuna", "Tonga", "Samoa", "American Samoa", "Niue", "Cook Islands", 
                   "French Polynesia", "United States Minor Outlying Islands", "Pitcairn Islands")
    land <- ne_countries(scale = "large", returnclass = "sf") %>% 
      dplyr::filter(name_en %in% land_list)
    indonesia <- eez %>% 
      filter(TERRITORY1 %in% c("Indonesia")) # remove Indonesia EEZ
    land_mask <- st_union(land, indonesia) # we also want to remove Indonesia EEZs at this point
    
    # Finally, remove land_mask from merged to end up with desired planning region
    Bndry <- fSpatPlan_Get_MaskedPolygon(merged, 0.25, land_mask, TRUE) 
    
    return(Bndry)
  }
  
  # Add Pacific
  
  
  # Add Indian
  
  
  # Add Atlantic
  
  
  # Add EEZs
  
  if (Limits == "Australia"){
    # Get Australia's EEZ
    Bndry <- st_read("Data/World_EEZ_v11/eez_v11.shp", quiet = TRUE) %>% 
      filter(TERRITORY1 == "Australia") %>% 
      st_transform(cCRS)
    return(Bndry)
  }
  
  
}
