# For the moment I am having to use the valid version of the shp file that Alvise gave me. I tried to use the original files,
# but the st_make_valid seems to do funny things. Converts the POLYGONS to MULTIPOLYGONS, which does something wierd when you 
# transform them

fCreate_PUs <- function(PU_size) {
source("Functions/SpatialPlanning/SpatPlan_Extras.R") # Load the extras, including functions and libraries

#  ### Set user parameters 
Limits = "Global"
Shape <- "Hexagon" # "Shape of PUs
cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # Robinson

# Plot the Valid data
file <- "Data/GMWValid/GMWValid.shp"

GMW <- st_read(file) %>%   #Read the GMW shapefile
  st_transform(crs = cCRS)

Bndry <- fSpatPlan_Get_Boundary(Limits, cCRS)

PUs <- fSpatPlan_Get_PlanningUnits(Bndry, Bndry, 40000, Shape, inverse = FALSE) %>% 
  st_make_valid()
  
logi_overlap <- PUs %>%
  st_overlaps(GMW) %>% 
  lengths > 0 # Get logical vector instead of sparse geometry binary

logi_contains <- PUs %>%
  st_contains(GMW) %>% 
  lengths > 0 # Get logical vector instead of sparse geometry binary

logi <- logi_overlap + logi_contains
logi[logi>1] <- 1

logi <- as.logical(logi)
PUsM <- PUs[logi, ]

saveRDS(PUsM, "RDS/Large_PUs_40000.rds")
}
