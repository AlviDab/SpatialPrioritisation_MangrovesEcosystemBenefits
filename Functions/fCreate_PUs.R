# Written by: Jason Everett
# Produce the planning units

fCreate_PUs <- function(PU_size) {
### Set user parameters 
Limits = "Global"
Shape <- "Hexagon" # "Shape of PUs
cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # Mollweide

source("Functions/fSpatPlan_Get_Boundary.R")
source("Functions/fSpatPlan_Get_PlanningUnits.R")

Bndry <- fSpatPlan_Get_Boundary(Limits, cCRS)

PUs <- fSpatPlan_Get_PlanningUnits(Bndry, Bndry, PU_size, Shape, inverse = FALSE) %>% 
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
}
