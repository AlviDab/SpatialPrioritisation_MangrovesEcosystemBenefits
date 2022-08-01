# Function that intersects PUs_provinces with features and creates "subspecies" of features according to PUs_provinces.
# Author: Tin Buenafe
# Last modified: 07-12-22

# Note, some of the features here wouldn't intersect with a particular province, so they would be automatically removed from the final object. If this isn't what you want to do, just comment out the second to last line of the code in the function written below.

# Needed inputs:
# 1. PUs_ID: with ID. This object should only have ID and geometry columns.
# 2. features: one sf object with all features as column names and value as 1s or 0s. Also make sure that there's a ID column.
# 3. PUs_provinces: province names should be in one column named 'province'. This object should only have province, ID, and geometry columns.

create_NewFeatProv <- function(PUs_ID, features, PUs_provinces) {
  
  # Load packages
  pacman::p_load(tidyverse, sf, terra, rnaturalearth)
  
  # For each feature, intersect them with the PUs_provinces
  piv <- list() # empty list
  for(i in 3:(ncol(features))) {  #-2 to remove geometry and ID
    #print(i)
    feat_name <- names(features)[i]  
    
    feat <- features %>% dplyr::select(ID, all_of(feat_name)) %>% 
      st_drop_geometry()
    
    int <- PUs_provinces %>% 
      st_drop_geometry() %>% 
      left_join(feat, ., by = "ID") %>% 
      dplyr::filter(!!sym(feat_name) != 0) #I keep only the values that are not zero
    
    piv[[i]] <- int %>% 
      pivot_wider(names_from = province, values_from = !!sym(feat_name), names_glue = "{feat_name}_{province}")
  }
  
  NewFeatures <- plyr::join_all(piv[3:67], by='ID', type='left')
  
  NewFeatures[is.na(NewFeatures)] <- 0
  
  NewFeatures[, which(colSums(NewFeatures %>% dplyr::select(-ID)) != 0)] # comment this line out if you don't want to remove new features made that didn't intersect with particular PUs_provinces
  
  NewFeatures <- NewFeatures %>%
    left_join(PUs_ID, ., by = "ID")
  
  return(NewFeatures)
  
}

