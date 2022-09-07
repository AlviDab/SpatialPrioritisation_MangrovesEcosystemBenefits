#Author: Dabalà Alvise
#18/01/2022

#Code to prepare all the layers before the prioritisation

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

############################################
#1. Set the various layer for the analysis

#1.1. Prepare the files

#1.1.1. Source the different function
source("Functions/fCreate_PUs.R") #Function to produce the PUs form the shapefile of the mangroves distribution
source("Functions/fIntersection_IUCNnearestfeature.R") #Function to intersect the PUs with the species distribution
source("Functions/fSelect_PUsArea.R") #Function to select the area of the PUs
source("Functions/fExtract_CarbonSequestration.R") #Function to select the area of the PUs
source("Functions/fCalculate_BioTypArea.R") #Function to select the area of the PUs
source("Functions/fIntersect_PointShp.R")
source("Functions/fRemove_NANearestNeighbourg.R")
source("Functions/fSelect_WDPA.R")
source("Functions/fRemoveNA_Coast.R")

#Set the projection
cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#1.1.2. Define the repository of the different data used
GMWtxt <- "Data/GMWValid/GMWValid.shp" #GMW mangrove distribution shapefile 
IUCNtxt <- "Data/IUCN/MANGROVES.shp" #IUCN mangroves range shapefile
Fisheriestxt <- "Data/Fisheries/fish_pres/w001001.adf" #Fisheries shapefile
Coasttxt <- "Data/ResultsFloodingMenendez/3_Teselas aggregation (20km)- Per Return Period and storm/MANGLAR_Global_TESELA_AEB_TOTAL.shp" #Coastal protection data
AGBtxt <- "Data/Canopy_Biomass/CMS_Global_Map_Mangrove_Canopy_1665/data/agb" #Directory of the aboveground carbon
SoilCarbontxt <- "Data/Sanderman_GMW_SoilCarbon/mangroves_SOC30m_0_100cm"
MarineProvincestxt <- "Data/MarineProvinces/01_Data/WCMC-036-MEOW-PPOW-2007-2012.shp" #Directory Marine Provinces
BioTyptxt <- "Data/Worthington/TNC-006_BiophysicalTypologyMangroves/01_Data/Mangrove_Typology_v2_2_2016.shp" #Shapefile mangroves biophysical typology

#1.2. Read the shapefiles and the rasters

#1.2.1 Read, clean and plot the shapefile of GMW

GMW <- GMWtxt %>%
   st_read() %>% #Read the GMW shapefile
   st_transform(crs = cCRS) #Project GMW to degrees to make the cut

#1.2.2. Read data for a map of the world

world_map <- ne_countries(scale = "large", returnclass = "sf") %>%
  st_transform(crs = cCRS) %>% #I project the crs of the GMW to meters
  st_make_valid() #I make the shapefile valid

#1.2.3. Read, clean and plot IUCN data
IUCN <- IUCNtxt %>% 
  st_read() %>% #I read the file of the IUCN
  st_transform(crs = cCRS) %>% #I re-project the shapefile to utm in meter 
  dplyr::filter(compiler == "IUCN") #I keep all the data that are compiled by IUCN, removing two species with wrong distribution

#1.2.4. Read marine provinces

MarineProvinces <- st_read(MarineProvincestxt) %>%
  st_transform(crs = cCRS) %>% 
  filter(TYPE == "MEOW") #Filter only the MEOW provinces

#1.2.5. Read mangroves biophysical typology

BioTyp <- st_read(BioTyptxt) %>%
  st_transform(crs = cCRS) #I project the crs of the GMW to meters

#1.2.6. Read, clean and plot cost Layer
Fish <- raster(Fisheriestxt) #I read the raster file of fisheries (used raster because with terra I was not able to read it)

crs(Fish) <- crs("+proj=cea +lat_ts=0 +lon_0=-160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") # I set the CRS of the file

Fish <- as(Fish, "SpatRaster") #Produce a terra raster file
  
#1.2.7 Read and clean data of coastal protection (Menéndez et al. 2020)
Coast <- st_read(Coasttxt) %>% #Read the shapefile
  st_transform(crs = cCRS) #Transform the crs

####################################
#2. Production of the planning units

#2.1.1 Produce the planning units
PUs <- fCreate_PUs(400) #Produce the planning units for the GMW selected (the number is the area in km^2)
  
Large_PUs <- fCreate_PUs(40000) #Produce planning units for the aggregated results
  
#2.2 Calculation of GMW area coverage of each PU
#    distribution of the areas
PUs <- fSelect_PUsArea(PUs, GMW) #The number is the percentage of area of mangrove
                                 #in the PUs to define that as a PUs
                                 #e.g. 1 means that 1/100 of the area of a PU should
                                 #be covered by mangroves to be defined as a PU

#2.3 Set the conservation features for each PUs

#2.3.1 IUCN Mangroves species distribution
result <- fIntersection_IUCNnearestfeature(PUs, IUCN) #Intersection between the 
                                                      #planning units and the IUCN Red list data
PUs <- result[[1]]
nPUs <- result[[2]] #number of PUs that intercept with each species distribution

#2.3.2 Marine Provinces
PUs$Province <- MarineProvinces$PROVINC[as_vector(st_nearest_feature(PUs, MarineProvinces))] #Define in what province you can find the PU

PUs$AreaGMWKm1 <- PUs$AreaGMWKm #Duplicate the column AreaGMWKm

PUs <- PUs %>% 
  as_tibble() %>% #Transform in a tibble
  pivot_wider(names_from = Province, values_from = AreaGMWKm1, values_fill = 0) %>%
  st_sf() #Transform in a sf

#2.3.3 Biophysical typology
BioTyp <- BioTyp %>%
  dplyr::select(Class, Sedimentar)

PUs <- fCalc_BioTypArea() #Calculate the area of each biophysical typology in a PU

#2.4 Set ecosystem services layers

#2.4.1 Fisheries benefits
#I remove the planning unit that create pproblems when reprojected
PUs_Fish <- PUs %>%
  mutate(ID = as.numeric(rownames(PUs))) %>% #Add a new column called ID with rownames as value for each row.
  slice(1:9110)

PUs_Fish <- PUs_Fish %>%
  st_transform(crs = "+proj=cea +lat_ts=0 +lon_0=-160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") %>% #I project the PUs to the crs of fisheries raster
  arrange(ID)

FishCost <- exact_extract(Fish, PUs_Fish, c('mean')) %>% #I extract the data of the Fisheries 
  tibble() %>% 
  mutate(ID = PUs_Fish$ID) %>% #I group by ID
  rename(Fishing_Intensity = 1)

PUs <- PUs %>%
  left_join(FishCost, by = "ID") #Column of the fishing intensity layer

#2.4.2 Coastal protection

#Calculate mean value of coastal protection for each PU
PUs <- PUs %>% 
  fIntersect_PointShp(Coast, c("TOT_STOCK", "POP"))

#2.4.3 Aboveground biomass
PUs <- Extract_Carbon(AGBtxt) %>%  #I extract the carbon cost
  rename(biomass_carbon = carbon)

#2.4.4 Soil carbon
PUs <- Extract_Carbon(SoilCarbontxt) %>% 
  rename(soil_carbon = carbon)

#2.5 Fill NA using nearest neighborhood

#2.5.1 Fill NA
#Fill NAs with Nearest neighbourghood
PUs <- PUs %>% 
  fNN_x(Fishing_Intensity) %>% 
  fNN_x(biomass_carbon) %>% 
  fNN_x(soil_carbon)

PUs <- PUs %>% 
  fNN_Coast_x()

PUs$Tot_Carbon <- (PUs$soil_carbon + PUs$biomass_carbon)*100 #Transform from Mg C ha^-1 to Mg C Km²

#####################################
#3. WDPA, countries and continents 

#3.1. Intersect planning units with WDPA dataset
PUs <- fSelectWDPA(PUs) %>% 
  mutate(Protected = as.logical(Protected))

#3.2. Intersect with countries and EEZ
#PUs by nation
EEZ <- st_read("Data/EEZ/eez_v11.shp") %>% 
  st_transform(cCRS)

a <- st_join(st_centroid(PUs), world_map, join = st_intersects) %>%
  dplyr::select(c(colnames(PUs), iso_a3_eh))

a <- st_join(a, EEZ, join = st_intersects) %>%
  dplyr::select(c(colnames(a), ISO_TER1))

b <- a %>% 
  mutate(sovereignt = case_when(is.na(iso_a3_eh) ~ ISO_TER1, 
                                TRUE ~ iso_a3_eh)) %>% 
  dplyr::select(ID, iso_a3_eh) %>% 
  st_drop_geometry() %>% 
  tibble() %>% 
  rename(country = iso_a3_eh)

PUs <- PUs %>% 
  left_join(b, by = "ID")

PUs <- fNN_x(PUs, country)

#3.3. PUs by Continent
library(countrycode)
countries <- data.frame(country = PUs$country)

PUs$continent <- countrycode(sourcevar = countries[, "country"],
            origin = "iso3c",
            destination = "continent")

PUs$country <- countrycode(sourcevar = countries[, "country"],
                           origin = "iso3c",
                           destination = "country.name")

# Solve overseas regions of France problem
GUF <- read("Data/Countries/GUF_adm0.shp") %>%
  st_transform(cCRS)

GLP <- st_read("Data/Countries/GLP_adm0.shp") %>%
  st_transform(cCRS)

ATF <- st_read("Data/Countries/ATF_adm0.shp") %>%
  st_transform(cCRS)

MYT <- st_read("Data/Countries/MYT_adm0.shp") %>%
  st_transform(cCRS)

KNA <- st_read("Data/Countries/KNA_adm0.shp") %>%
  st_transform(cCRS)

ATG <- st_read("Data/Countries/ATG_adm0.shp") %>%
  st_transform(cCRS)

DMA <- st_read("Data/Countries/DMA_adm0.shp") %>%
  st_transform(cCRS)

a <- st_as_sf(result_BioServ_WDPA) %>%
  filter(country == "France")

a <- a %>%
  mutate(country = (case_when(
    (lengths(st_intersects(a, GUF)) >= 1) ~ "GUF",
    (lengths(st_intersects(a, GLP)) >= 1) ~ "GLP",
    (lengths(st_intersects(a, ATF)) >= 1) ~ "ATP",
    (lengths(st_intersects(a, MYT)) >= 1) ~ "MYT",
    (lengths(st_intersects(a, KNA)) >= 1) ~ "KNA",
    (lengths(st_intersects(a, ATG)) >= 1) ~ "ATG",
    (lengths(st_intersects(a, DMA)) >= 1) ~ "DMA",
    TRUE ~ a$country
  ))) %>%
  mutate(country = (na_if(country, "France")))

a <- fNN_x(a, country)

a$continent <- countrycode(sourcevar = a$country,
                           origin = "iso3c",
                           destination = "continent")

a$country <- countrycode(sourcevar = a$country,
                           origin = "iso3c",
                           destination = "country.name")

result_BioServ_WDPA <- result_BioServ_WDPA %>% 
  filter(country != "France") %>% 
  add_row(a) %>% 
  arrange(ID)

#3.5. Solve Netherlands problem
result_BioServ_WDPA <- result_BioServ_WDPA %>% 
  mutate(continent = case_when(country == "Netherlands" ~ "Americas", 
                   TRUE ~ PUs$continent))

#transform all the values to numeric and the values <1e-6 to 0
 PUs <- PUs %>% 
   mutate(across(!geometry & !Protected & !country & !continent, 
                 as.numeric)) %>% 
   mutate(across(!geometry & !Protected & !country & !continent,
                 ~ case_when(. <= 1e-6 ~ 0, 
                             TRUE ~ .)))
 
 ###############################################################################
 #4. SaveRDS
 saveRDS(Large_PUs, "RDS/Large_PUs.rds")
 saveRDS(PUs, "RDS/PUs_NotSplitted.rds")