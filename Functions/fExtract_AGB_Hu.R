# Function to extract biomass carbon from Hu et al. 2020

library(terra)
library(exactextractr)
library(raster)
library(mapview)

function(PUs) {
  #Load the data
  AGB <- rast("Data/AGB_Hu_2020/global_mangrove_agb.tif")
  
  #Transform the PUs to the projection of the raster
  PUs <- PUs %>% 
    st_transform(crs = 4326) %>% 
    st_make_valid()
  
  #Plot it
  mapview(PUs_NA) + mapview(AGB, maxpixels =  293571135)
  
  #I extract the mean value of AGB for each PU
  AGB_Hu <- exact_extract(AGB, PUs, c('mean')) %>% #I extract the data of the Fisheries (I have to make PUs compatible with a terra using vect)
    tibble() %>% 
    mutate(ID = PUs$ID) %>% #I group by ID
    rename(AGB_Hu = 1)
  
  #Join AGB_Hu with PUs
  PUs_NA <- PUs_NA %>% 
    left_join(AGB_Hu/100, by = "ID")
  
  #Check number of shared NAs
  PUs_NA %>% 
    dplyr::filter(is.na(AGB_Hu) & is.na(biomass_carbon)) %>% 
    summarise(n())
  
  #Differences between the databases
  PUs_NA <- PUs_NA %>% 
    mutate(diff_AGB = AGB_Hu/100 - biomass_carbon)
  
  hist(PUs_NA$diff_AGB)
}