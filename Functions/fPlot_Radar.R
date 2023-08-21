#Author: Alvise Dabalà
#Function to produce a radar plot
#Input: 
# -x: dataframe that values of ecosystem services
fPlot_Radar <- function(x) {
    
  devtools::install_github("ricardo-bion/ggradar", 
                           dependencies = TRUE)
  pacman::p_load(ggradar, scales)
  
  plt <- x %>%
    ggradar(grid.label.size = 2.5,  # Affects the grid annotations (0%, 50%, etc.)
      axis.label.size = 1, # Afftects the names of the variables
      group.point.size = 1,  # Simply the size of the point 
      group.line.width = 0.5,
      group.colours = c("#482274", "#355F8D", "#21908D"),
      background.circle.colour = "NA",
      gridline.mid.colour = "grey",
      legend.position = "none"
    )
}
