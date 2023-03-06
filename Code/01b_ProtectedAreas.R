# load packages
library(wdpar)
library(dplyr)
library(sf)

#Set the projection
cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
PUs <- readRDS("RDS/PUs_Splitted.rds")

#Load Protected area file1
WDPA1 <- st_read("Data/WDPA_poly/WDPA_WDOECM_Feb2022_Public_all_shp-polygons1.shp")

#Transform PUs to the same CRS
PUs <- PUs %>% 
  st_transform(crs = "EPSG:4326")

#Remove invalid features that are 475, 3690, 4458, 17087, 56357, then 56353, then 9111, then 9111
#Find invalid features
logi_int <- st_is_valid(WDPA1)

WDPA1_invalid <- WDPA1[logi_int == FALSE,]

WDPA1 <- WDPA1[logi_int == TRUE,]

#Remove the invalid PU because of the transformation to a different CRS
#Then I will add the information downloading the data at national scale
PUs <- PUs[c(1:9110),]
  
#Keep only the WDPA data that intersect with the PUs
PUs_WDPA1 <- WDPA1 %>% 
  st_intersects(PUs)

logi_int <- lengths(PUs_WDPA1) > 0
WDPA1 <- WDPA1[logi_int == TRUE,]

#Save the resulting shapefile
saveRDS(WDPA1, "RDS/WDPA1_filtered.rds")
saveRDS(WDPA1_invalid, "RDS/WDPA1_invalid.rds")

#Remove WDPA1
rm(WDPA1)

sf_use_s2(FALSE)

#Make the invalid geometries valid
WDPA1_valid <- st_repair_geometry(WDPA1_invalid)

#Keep only the valid WDPA data that intersect with the PUs
PUs_WDPA1 <- WDPA1_valid %>% 
  st_intersects(PUs)

logi_int <- lengths(PUs_WDPA1) > 0
WDPA1_valid <- WDPA1_valid[logi_int == TRUE,] #no intersections so I do not save the file

sf_use_s2(TRUE)

#Rm the file that I will not use now
rm(PUs_WDPA1)
rm(WDPA1_invalid)
rm(WDPA1_valid)

#Load Protected area file2
WDPA2 <- st_read("Data/WDPA_poly/WDPA_WDOECM_Feb2022_Public_all_shp-polygons2.shp")

#Remove invalid features
#Find invalid features
logi_int <- st_is_valid(WDPA2)

WDPA2_invalid <- WDPA2[logi_int == FALSE,]

WDPA2 <- WDPA2[logi_int == TRUE,]

#Keep only the WDPA data that intersect with the PUs
PUs_WDPA2 <- WDPA2 %>% 
  st_intersects(PUs)

logi_int <- lengths(PUs_WDPA2) > 0
WDPA2 <- WDPA2[logi_int == TRUE,]

#Save the resulting shapefile
saveRDS(WDPA2, "RDS/WDPA2_filtered.rds")
saveRDS(WDPA2_invalid, "RDS/WDPA2_invalid.rds")

#Remove WDPA2
rm(WDPA2)

sf_use_s2(FALSE)

#Make the invalid geometries valid
WDPA2_valid <- st_repair_geometry(WDPA2_invalid)

#Keep only the valid WDPA data that intersect with the PUs
PUs_WDPA2 <- WDPA2_valid %>% 
  st_intersects(PUs)

logi_int <- lengths(PUs_WDPA2) > 0
WDPA2_valid <- WDPA2_valid[logi_int == TRUE,] #no intersections so I do not save the file

sf_use_s2(TRUE)

#Rm the file that I will not use now
rm(PUs_WDPA2)
rm(WDPA2_invalid)
rm(WDPA2_valid)

#Load Protected area file3
WDPA3 <- st_read("Data/WDPA_poly/WDPA_WDOECM_Feb2022_Public_all_shp-polygons3.shp")

#Remove invalid features
#Find invalid features
logi_int <- st_is_valid(WDPA3)

WDPA3_invalid <- WDPA3[logi_int == FALSE,]

WDPA3 <- WDPA3[logi_int == TRUE,]

#Keep only the WDPA data that intersect with the PUs
PUs_WDPA3 <- WDPA3 %>% 
  st_intersects(PUs)

logi_int <- lengths(PUs_WDPA3) > 0
WDPA3 <- WDPA3[logi_int == TRUE,]

#Save the resulting shapefile
saveRDS(WDPA3, "RDS/WDPA3_filtered.rds")
saveRDS(WDPA3_invalid, "RDS/WDPA3_invalid.rds")

#Remove WDPA3
rm(WDPA3)

sf_use_s2(FALSE)

#Keep only the valid WDPA data that intersect with the PUs
PUs_WDPA3 <- WDPA3_invalid %>% 
  st_intersects(PUs)

logi_int <- lengths(PUs_WDPA3) > 0
WDPA3_invalid <- WDPA3_invalid[logi_int == TRUE,] #no intersections so I do not save the file

sf_use_s2(TRUE)

WDPA3_valid <- st_repair_geometry(WDPA3_invalid)

#Rm the file that I will not use now
rm(PUs_WDPA3)
rm(WDPA3_invalid)

#Save
saveRDS(WDPA3_valid, "RDS/WDPA3_valid.rds")

#Read the three shapefiles
WDPA1 <- readRDS("RDS/WDPA1_filtered.rds")
WDPA2 <- readRDS("RDS/WDPA2_filtered.rds")
WDPA3 <- readRDS("RDS/WDPA3_filtered.rds")
WDPA3_valid <- readRDS("RDS/WDPA3_valid.rds")

#Make a single file
WDPA_123 <- WDPA1 %>% 
  add_row(WDPA2) %>% 
  add_row(WDPA3) %>% 
  add_row(WDPA3_valid) %>% 
  st_transform(crs = 4326)

#Remove the other WDPA
rm(WDPA1)
rm(WDPA2)
rm(WDPA3)

logi_int <- st_is_valid(WDPA_123)

WDPA_123_invalid <- WDPA_123[logi_int == FALSE,]

WDPA_123 <- WDPA_123[logi_int == TRUE,]

#Remove unesco reserve and only include designated, inscribed or established PAs
WDPA_123_clean <- WDPA_123 %>% 
  dplyr::filter(!(DESIG_ENG %in% c("UNESCO-MAB Biosphere Reserve"))) %>% 
  dplyr::filter(STATUS %in% c("Designated", "Inscribed", "Established"))

#Save the file
saveRDS(WDPA_123_clean, "RDS/WDPA_123_clean.rds")

#Add the point geometries
WDPA_p_1 <- st_read("Data/WDPA_poly/shp_points_1/WDPA_WDOECM_Feb2022_Public_all_shp-points.shp")
WDPA_p_2 <- st_read("Data/WDPA_poly/shp_points_2/WDPA_WDOECM_Feb2022_Public_all_shp-points.shp")
WDPA_p_3 <- st_read("Data/WDPA_poly/shp_points_3/WDPA_WDOECM_Feb2022_Public_all_shp-points.shp")

WDPA_p_123 <- WDPA_p_1 %>% 
  add_row(WDPA_p_2) %>% 
  add_row(WDPA_p_3)

#Remove unesco reserve and only include designated, inscribed or established PAs
WDPA_p_123_clean <- WDPA_p_123 %>% 
  dplyr::filter(!(DESIG_ENG %in% c("UNESCO-MAB Biosphere Reserve"))) %>% 
  dplyr::filter(STATUS %in% c("Designated", "Inscribed", "Established"))

## remove protected areas represented as points that do not have a reported area
is_point <- vapply(sf::st_geometry(WDPA_p_123_clean), inherits, logical(1),
                   c("POINT", "MULTIPOINT"))
WDPA_p_123_clean$GEOMETRY_TYPE <- "POLYGON"
WDPA_p_123_clean$GEOMETRY_TYPE[is_point] <- "POINT"

WDPA_p_123_clean <- WDPA_p_123_clean[!(WDPA_p_123_clean$GEOMETRY_TYPE == "POINT" & !is.finite(WDPA_p_123_clean$REP_AREA)), ]

x_points_pos <- which(WDPA_p_123_clean$GEOMETRY_TYPE == "POINT")
if (length(x_points_pos) > 0) {
  x_points_data <- WDPA_p_123_clean[x_points_pos, ]
  x_points_data <- sf::st_buffer(x_points_data,
                                 sqrt((x_points_data$REP_AREA * 1e6) / pi))
  if (any(WDPA_p_123_clean$GEOMETRY_TYPE == "POLYGON")) {
    WDPA_p_123_clean <- rbind(WDPA_p_123_clean[which(x$GEOMETRY_TYPE == "POLYGON"), ], x_points_data)
  } else {
    WDPA_p_123_clean <- x_points_data
  }
  WDPA_p_123_clean <- sf::st_set_precision(WDPA_p_123_clean, 1500)
}

#Keep only the wdpa that intersect with the PUs
WDPA_p_123_PUs <- WDPA_p_123_clean %>%
  st_make_valid() %>% 
  st_intersects(PUs)

logi_int <- lengths(WDPA_p_123_PUs) > 0
WDPA_p_123_clean <- WDPA_p_123_clean[logi_int == TRUE,]

#Add column geometry type also for WDPA_123_clean
is_point <- vapply(sf::st_geometry(WDPA_123_clean), inherits, logical(1),
                   c("POINT", "MULTIPOINT"))
WDPA_123_clean$GEOMETRY_TYPE <- "POLYGON"
WDPA_123_clean$GEOMETRY_TYPE[is_point] <- "POINT"

WDPA_123_clean <- WDPA_123_clean[!(WDPA_123_clean$GEOMETRY_TYPE == "POINT" & !is.finite(WDPA_123_clean$REP_AREA)), ]

WDPA_123_clean <- WDPA_123_clean %>% 
  add_row(WDPA_p_123_clean)

saveRDS(WDPA_123_clean, "RDS/WDPA_polygon_points_123_clean.rds")

#Read again the PUs
PUs <- readRDS("RDS/PUs_Splitted.rds") %>% 
  st_transform(cCRS)

source("Functions/fSelect_AllWDPA.R")

#Calculate how much of the planning units are protected
PUs <- fSelect_AllWDPA(PUs) %>% 
  mutate(AllProtected = as.logical(AllProtected))

#Now incorporate protection of PUs 9111 downloading fiji protected areas
wdpa_FJI0 <- st_read("Data/WDPA_poly/WDPA_WDOECM_Dec2022_Public_FJI_shp_0/WDPA_WDOECM_Dec2022_Public_FJI_shp-polygons.shp")
wdpa_FJI1 <- st_read("Data/WDPA_poly/WDPA_WDOECM_Dec2022_Public_FJI_shp_1/WDPA_WDOECM_Dec2022_Public_FJI_shp-polygons.shp")
wdpa_FJI2 <- st_read("Data/WDPA_poly/WDPA_WDOECM_Dec2022_Public_FJI_shp_2/WDPA_WDOECM_Dec2022_Public_FJI_shp-polygons.shp")

wdpa_FJI0_p <- st_read("Data/WDPA_poly/WDPA_WDOECM_Dec2022_Public_FJI_shp_0/WDPA_WDOECM_Dec2022_Public_FJI_shp-points.shp")
wdpa_FJI1_p <- st_read("Data/WDPA_poly/WDPA_WDOECM_Dec2022_Public_FJI_shp_1/WDPA_WDOECM_Dec2022_Public_FJI_shp-points.shp")
wdpa_FJI2_p <- st_read("Data/WDPA_poly/WDPA_WDOECM_Dec2022_Public_FJI_shp_2/WDPA_WDOECM_Dec2022_Public_FJI_shp-points.shp")

wdpa_FJI <- wdpa_FJI0 %>% 
  add_row(wdpa_FJI1) %>% 
  add_row(wdpa_FJI2) %>% 
  add_row(wdpa_FJI0_p) %>% 
  add_row(wdpa_FJI1_p) %>% 
  add_row(wdpa_FJI2_p)

rm(wdpa_FJI0)
rm(wdpa_FJI1)
rm(wdpa_FJI2)
rm(wdpa_FJI0_p)
rm(wdpa_FJI1_p)
rm(wdpa_FJI2_p)

#Clean wdpa FJI
wdpa_FJI <- wdpa_clean(wdpa_FJI)

saveRDS(wdpa_FJI, "RDS/wdpa_FJI.rds")

wdpa_FJI <- readRDS("RDS/wdpa_FJI.rds")

PUs_9111 <- PUs[9111,]

#Check intersection IUCN I-IV
wdpa_FJI_I_IV <- wdpa_FJI %>% 
  filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV"))

wdpa_FJI_I_IV %>% 
  st_transform(cCRS) %>% 
  st_intersects(PUs_9111) #There are no intersections

#Open GMW
GMW <- st_read("Data/GMWValid/GMWValid.shp") %>% 
  st_transform(cCRS)

wdpa_FJI <- wdpa_FJI %>% 
  st_transform(crs = cCRS)

wdpa_FJI_PUs_9111 <- wdpa_FJI %>% 
  st_intersects(PUs_9111)

logi_int <- lengths(wdpa_FJI_PUs_9111) > 0
wdpa_FJI <- wdpa_FJI[logi_int == TRUE,] #no intersections so I do not save the file

#Make the union of the PAs
wdpa_FJI <- wdpa_FJI %>% 
  st_transform(cCRS) %>%
  st_make_valid() %>% 
  st_union() %>% 
  st_cast("POLYGON")

GMW_PUs_9111 <- GMW %>% 
  st_intersects(PUs_9111)

logi_int <- lengths(GMW_PUs_9111) > 0
GMW <- GMW[logi_int == TRUE,] 

saveRDS(GMW, "RDS/GMW_PUs_9111.rds")

#Calculate the intersection between the mangrove of each PU with the protected areas
PUs9111_GMW_WDPA <- PUs_9111 %>% 
  st_intersection(GMW) %>% 
  st_intersection(wdpa_FJI)

#Calculate the area of the intersection
PUs9111_GMW_WDPA$AreaWDPA_I_VI <- PUs9111_GMW_WDPA %>% 
  st_area() %>%
  units::set_units(km^2)

#Summarise the area covered by protected areas for each PU
PUs9111_GMW_WDPA <- PUs9111_GMW_WDPA %>%
  as_tibble() %>%
  group_by(ID) %>%
  summarise(AreaWDPA_I_VI = sum(AreaWDPA_I_VI)) %>%
  mutate(AreaWDPA_I_VI = ifelse(is.na(AreaWDPA_I_VI), 0, AreaWDPA_I_VI))

PUs[9111, "AreaWDPA_I_VI"] <- PUs9111_GMW_WDPA$AreaWDPA_I_VI

#Areas with more than 50% of mangrove area covered by mangroves are defined as protected
PUs$Protected_I_VI <- ifelse(as.numeric(PUs$AreaWDPA_I_VI) < as.numeric(PUs$AreaGMWKm)/2, "FALSE", "TRUE")

dir.create("RDS_rr")
saveRDS(PUs, "RDS_rr/PUs_Splitted_I_IV_and_All_9111.rds")
