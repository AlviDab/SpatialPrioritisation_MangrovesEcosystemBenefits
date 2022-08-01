# Function to create hexagonal planning units for your area of interest of GMW.
# Inputs needed are:
#   GMW: mangrove distribution shapefile
#   Area: dimension of the PU in km^2

fPUs_GMW <- function (GMW, Area) {

#Define cell area and shape
CellArea <- Area
Shape <- "Hexagon"

#Source the function for the PUs and generate the PUs
source("Functions/fCreate_PlanningUnits.R")
PUs <- fCreate_PlanningUnits(GMW, CellArea, Shape)

#Define the crs of the PUs
PUs <- st_transform(PUs, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

return(PUs)
}