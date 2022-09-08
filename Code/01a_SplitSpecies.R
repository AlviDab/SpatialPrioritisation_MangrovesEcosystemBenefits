#Author: Dabalà Alvise
#10/07/2022
#Split the species distribution by biophysical typology and marine province

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

#Set the projection
cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#Open the PUs
PUs <- readRDS("RDS/PUs_NotSplitted.rds") #Open planning units that are not split by biotyp and marine provinces

#I only keep the ID of the PUs
PUs_ID <- PUs %>% 
  dplyr::select(ID) #Select only the ID column

#Planning units species
PUs_species <- PUs %>% 
  dplyr::select(1:67) #Select only the column with the species of mangroves, the ID and the geometry

################################################################################
#split by province

#Read marine provinces shapefile
MarineProvincestxt <- "Data/MarineProvinces/01_Data/WCMC-036-MEOW-PPOW-2007-2012.shp" #Directory Marine Provinces

MarineProvinces <- st_read(MarineProvincestxt) %>% #Read marine provinces of the world
  st_transform(crs = cCRS) %>% #Transform to Mollweide CRS
  filter(TYPE == "MEOW") #Filter only the MEOW provinces

#Column with the province where we find each planning unit
PUs_provinces <- PUs_ID %>% 
  mutate(province = MarineProvinces$PROVINC[
    as_vector(st_nearest_feature(PUs, MarineProvinces))
    ]) #Make a column with the province of each PU

piv <- list() #Create a list

#Split by province
for(i in 3:(ncol(PUs_species))) {  #For across all the species column
  feat_name <- names(PUs_species)[i]  #Name of the species
  
  feat <- PUs_species %>% 
    dplyr::select(ID, all_of(feat_name)) %>% #Select ID column and the column of the species i
    st_drop_geometry() #drop the geometry column
  
  int <- PUs_provinces %>%
    st_drop_geometry() %>% #drop the geometry
    left_join(feat, ., by = "ID") #left join all the columns of feat by ID
  
  piv[[i-2]] <- int %>% #add int as i-2 element of the list
    pivot_wider(names_from = province, #get the names from the column province
                values_from = !!sym(feat_name), #get the values from the column of the species
                names_glue = "{feat_name}_{province}") #glue the name of the species to the name of the province
}

PUs_splitted <- plyr::join_all(piv, by='ID', type='left') #left join all the tibble in the list piv

PUs_splitted[is.na(PUs_splitted)] <- 0 #replace all the NAs in PUs_splitted to 0

PUs_splitted <- PUs_splitted %>% 
  dplyr::select(where( ~ is.numeric(.x) && sum(.x) != 0)) #Keep all the column that are numeric and which sum is != 0

################################################################################
#2. Biotyp
PUs_biotyp <- PUs_ID

typ <- c("Delta", "Estuary", "Lagoon", "OpenCoast", "Terrigenous", "Carbonate") #Define the name of all the biotyp

#2.1. Delta
PUs_int <- PUs_species %>% 
  mutate(across(!c(geometry, ID), ~ replace(., (. > 0), PUs$Delta[(. > 0)]))) #replace all the values that are not 0 with the values in the Delta column in the same row

PUs_int <- PUs_int %>% 
  st_drop_geometry() %>% #Drop the geometry
  as_tibble() %>% #Transform to tibble
  rename_with(~paste0(., "_", "Delta"), !ID) #Rename each column adding _Delta after the name of the species

PUs_biotyp <- PUs_biotyp %>% 
  left_join(PUs_int, by = "ID") #Left join the columns by ID

#2.2. Estuary
PUs_int <- PUs_species %>% 
  mutate(across(!c(geometry, ID), ~ replace(., (. > 0), PUs$Estuary[(. > 0)]))) #Replace all the values that are >0 with the values of the column Estuary in the same row

PUs_int <- PUs_int %>%
  st_drop_geometry() %>% #Drop geometry
  as_tibble() %>% 
  rename_with(~paste0(., "_", "Estuary"), !ID) #Add _Estuary at the end of each column

PUs_biotyp <- PUs_biotyp %>% 
  left_join(PUs_int, by = "ID") #Left join the columns by ID

#2.3. Lagoon
PUs_int <- PUs_species %>% 
  mutate(across(!c(geometry, ID), ~ replace(., (. > 0), PUs$Lagoon[(. > 0)]))) #Replace all the values that are >0 with values from the column Lagoon in the same row

PUs_int <- PUs_int %>% 
  st_drop_geometry() %>% #Drop geometry
  as_tibble() %>% 
  rename_with(~paste0(., "_", "Lagoon"), !ID) #Add _Lagoon at the end of each column

PUs_biotyp <- PUs_biotyp %>% 
  left_join(PUs_int, by = "ID") #Left join by ID

#2.4. Open Coast
PUs_int <- PUs_species %>% 
  mutate(across(!c(geometry, ID), ~ replace(., (. > 0), PUs$OpenCoast[(. > 0)]))) #Replace all the values that are >0 with values from the column OpenCoast in the same row

PUs_int <- PUs_int %>% 
  st_drop_geometry() %>% #Drop the geometry
  as_tibble() %>% 
  rename_with(~paste0(., "_", "OpenCoast"), !ID) #Add _OpenCoast at the end of each column

PUs_biotyp <- PUs_biotyp %>% 
  left_join(PUs_int, by = "ID")

#2.5. Terrigenous
PUs_int <- PUs_species %>% 
  mutate(across(!c(geometry, ID), ~ replace(., (. > 0), PUs$Terrigenous[(. > 0)]))) #Replace all the values that are >0 with values from the column Terrigenous in the same row

PUs_int <- PUs_int %>% 
  st_drop_geometry() %>% #Drop geometry
  as_tibble() %>% 
  rename_with(~paste0(., "_", "Terrigenous"), !ID) #Left join and add _Terrigenous to the name of all the columns

PUs_biotyp <- PUs_biotyp %>% 
  left_join(PUs_int, by = "ID")

#2.6. Carbonate
PUs_int <- PUs_species %>% 
  mutate(across(!c(geometry, ID), ~ replace(., (. > 0), PUs$Carbonate[(. > 0)]))) #Replace all the values that are >0 with values from the column Carbonate in the same row

PUs_int <- PUs_int %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  rename_with(~paste0(., "_", "Carbonate"), !ID) #Left join and add _Carbonate to the name of all the columns

PUs_biotyp <- PUs_biotyp %>% 
  left_join(PUs_int, by = "ID") %>% 
  dplyr::select(where( ~ is.numeric(.x) && sum(.x) != 0)) #Remove all the columns that are all 0

################################################################################
#Join the columns
PUs_splitted <- PUs_splitted %>% 
  left_join(st_drop_geometry(PUs_biotyp), by = "ID") #Left join all the columns PUs_biotyp

#Remove columns before splitting and add new columns
PUs <- PUs %>% 
  dplyr::select(-c(unique(PUs_provinces$province), 
                   unique(names(st_drop_geometry(PUs_species[,3:67]))), 
                   "Delta", "Estuary", "Lagoon", "OpenCoast", "Terrigenous", "Carbonate")) %>% 
  left_join(PUs_splitted, by = "ID") #Add splitted columns

#Save the results
saveRDS(PUs, "RDS/PUs_Splitted.rds")
