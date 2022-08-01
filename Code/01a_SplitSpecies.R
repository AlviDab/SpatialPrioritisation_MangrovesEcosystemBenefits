#Split the species distribution by biophysical typology and marine province
#10/07/2022

#Open all the packages needed
library(tidyverse)
library(sf)
library(knitr)
library(terra)
library(raster)
library(prioritizr)
library(units)
library(patchwork)
library(mapview)
library(viridis)
library(ggthemes)
library(rnaturalearth)
library(rgdal)
library(tmap)

#Open the PUs
readRDS(PUs, "RDS/PUs_NotSplitted.rds")

#I only keep the ID of the PUs
PUs_ID <- PUs %>% 
  dplyr::select(ID)

#Planning units species
PUs_species <- PUs %>% 
  dplyr::select(1:67) 

################################################################################
#split by province

#Read marine provinces shapefile
MarineProvincestxt <- "Data/MarineProvinces/01_Data/WCMC-036-MEOW-PPOW-2007-2012.shp" #Directory Marine Provinces

MarineProvinces <- st_read(MarineProvincestxt) %>%
  st_transform(crs = cCRS) %>% 
  filter(TYPE == "MEOW") #Filter only the MEOW provinces

#Column with the province where we find each planning unit
PUs_provinces <- PUs_ID %>% 
  mutate(province = MarineProvinces$PROVINC[
    as_vector(st_nearest_feature(PUs, MarineProvinces))
    ])

piv <- list()

#Split by province
for(i in 3:(ncol(PUs_species))) {  
  feat_name <- names(PUs_species)[i]  
  
  feat <- PUs_species %>% dplyr::select(ID, all_of(feat_name)) %>% 
    st_drop_geometry()
  
  int <- PUs_provinces %>% 
    st_drop_geometry() %>% 
    left_join(feat, ., by = "ID") 
  
  piv[[i-2]] <- int %>% 
    pivot_wider(names_from = province, values_from = !!sym(feat_name), names_glue = "{feat_name}_{province}")
}

PUs_splitted <- plyr::join_all(piv, by='ID', type='left')

PUs_splitted[is.na(PUs_splitted)] <- 0

PUs_splitted <- PUs_splitted %>% 
  select_if(~ !is.numeric(.) || sum(.) != 0)

################################################################################
#2. Biotyp
PUs_biotyp <- PUs_ID

typ <- c("Delta", "Estuary", "Lagoon", "OpenCoast", "Terrigenous", "Carbonate")

#2.1. Delta
PUs_int <- PUs_species %>% 
  mutate(across(!c(geometry, ID), ~ replace(., (. > 0), PUs$Delta[(. > 0)])))

PUs_int <- PUs_int %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename_with(~paste0(., "_", "Delta"), !ID)

PUs_biotyp <- PUs_biotyp %>% 
  left_join(PUs_int, by = "ID")

#2.2. Estuary
PUs_int <- PUs_species %>% 
  mutate(across(!c(geometry, ID), ~ replace(., (. > 0), PUs$Estuary[(. > 0)])))

PUs_int <- PUs_int %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename_with(~paste0(., "_", "Estuary"), !ID)

PUs_biotyp <- PUs_biotyp %>% 
  left_join(PUs_int, by = "ID")

#2.3. Lagoon
PUs_int <- PUs_species %>% 
  mutate(across(!c(geometry, ID), ~ replace(., (. > 0), PUs$Lagoon[(. > 0)])))

PUs_int <- PUs_int %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename_with(~paste0(., "_", "Lagoon"), !ID)

PUs_biotyp <- PUs_biotyp %>% 
  left_join(PUs_int, by = "ID")

#2.4. Open Coast
PUs_int <- PUs_species %>% 
  mutate(across(!c(geometry, ID), ~ replace(., (. > 0), PUs$OpenCoast[(. > 0)])))

PUs_int <- PUs_int %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename_with(~paste0(., "_", "OpenCoast"), !ID)

PUs_biotyp <- PUs_biotyp %>% 
  left_join(PUs_int, by = "ID")

#2.5. Terrigenous
PUs_int <- PUs_species %>% 
  mutate(across(!c(geometry, ID), ~ replace(., (. > 0), PUs$Terrigenous[(. > 0)])))

PUs_int <- PUs_int %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename_with(~paste0(., "_", "Terrigenous"), !ID)

PUs_biotyp <- PUs_biotyp %>% 
  left_join(PUs_int, by = "ID")

#2.6. Carbonate
PUs_int <- PUs_species %>% 
  mutate(across(!c(geometry, ID), ~ replace(., (. > 0), PUs$Carbonate[(. > 0)])))

PUs_int <- PUs_int %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename_with(~paste0(., "_", "Carbonate"), !ID)

PUs_biotyp <- PUs_biotyp %>% 
  left_join(PUs_int, by = "ID")

################################################################################
#Join the columns
PUs_splitted <- PUs_splitted %>% 
  left_join(st_drop_geometry(PUs_biotyp), by = "ID")

#Remove previous columns
PUs <- PUs %>% 
  dplyr::select(-c(unique(PUs_provinces$province), 
                   unique(names(st_drop_geometry(PUs_species[,3:67]))), 
                   "Delta", "Estuary", "Lagoon", "OpenCoast", "Terrigenous", "Carbonate")) %>% 
  left_join(PUs_splitted, by = "ID")

#Remove all the columns that are all zeros
PUs <- PUs %>% 
  select_if(~ !is.numeric(.) || sum(.) != 0)

#Save the results
saveRDS(PUs, "RDS/Mollweide/PUs_SplittedSpecies.rds")  
