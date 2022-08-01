
# Function to create square or heaxagonal planning units for your area of interest.
# Inputs needed are:
#   Bndry: An sf polygon object which outlines the limits of the study area.
#       The code takes the bbbox so the limits are the most important.
#       The output inherits the crs from this sf object so ensure it is in the correct projection for your needs
#   LandMass: An sf multipolygon object which contains all the areas (ie land) that you wish to remove from the grid.
#       The code assumes that any Planning Unit whose centroids is over land will be removed. This approximates > 50% of the PU is landward.
#   CellArea: The area in km you wish your resultant Planning Units to be.
#   Shape: Hexagon or Square
#
# Written by Jason Everett (UQ/UNSW/CSIRO)
# Written: 15 December 2020
# Updated: 11th February 2021

fCreate_PlanningUnits <- function(Bndry, CellArea, Shape){
  
  if(Shape %in% c("hexagon", "Hexagon")){
    sq <- FALSE
    diameter <- 2 * sqrt((CellArea*1e6)/((3*sqrt(3)/2))) * sqrt(3)/2 # Diameter in m's
  }
  
  if(Shape %in% c("square", "Square")){
    sq < TRUE
    diameter <- sqrt(CellArea*1e6) # Diameter in m's
  }
  
  # First create planning units for the whole region
  PUs <- st_make_grid(Bndry,
                      square = sq,
                      cellsize = c(diameter, diameter),
                      what = "polygons") %>%
    st_sf()
  
  # Check cell size worked ok.
  print(paste0("Range of cellsize are ",
               round(as.numeric(range(units::set_units(st_area(PUs), "km^2")))[1])," km2 to ",
               round(as.numeric(range(units::set_units(st_area(PUs), "km^2")))[2])," km2")) # Check area
  
  # First get all the PUs partially/wholly within the planning region
  logi_Reg <- PUs %>%
    st_intersects(Bndry) %>%
    lengths > 0 # Get logical vector instead of sparse geometry binary
  
  
  PUs <- PUs[logi_Reg == TRUE, ]
  
  return(PUs)
}