# Author of the code: Alvise Dabalà 
# Code is adapted from: https://r-graph-gallery.com/circular-barplot.html
# df should have the following feature, value, group
# group colors
# group legends

# ----- Functions for creating circular barplot -----
# Inputs:
# 1. df: data frame should have the following column names: feature, value, group
# feature: feature bars
# value: value plotted in the y-axis
# group: grouping factors
# 2. legend_color: vector list of colors; should have the group names and their corresponding colors
# 3. legend_list: list of groups/legends of groups

fPlot_Circular <- function(df, ext_val, colr, lab, lvl) {
  library(tidyverse)
  library(viridis)
  
  # Create dataset
  data <- df
  
  data$group <- factor(data$group)
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 3
  nObsType <- nlevels(as.factor(data$target))
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
  data <- rbind(data, to_add)
  data <- data %>% 
    arrange(group, feature)
  data$id <- rep(seq(1, nrow(data)/nObsType) , each=nObsType)
  
  # Get the name and the y position of each label
  label_data <- data %>% group_by(id, feature) %>% summarize(tot = sum(value))
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data <- data %>% 
    group_by(group) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data[1,2] <- grid_data[1,3]
  grid_data[1,3] <- grid_data[1,3] + 2
  #grid_data <- grid_data[-1,]
  
  # Make the plot
  p <- ggplot(data) +      
    # Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=value, fill = factor(target, levels=lvl)), position = "stack", stat="identity", alpha=1) +
    scale_fill_manual(name = "",
                      values = colr) +
    
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "black", alpha=1, size=0.3, inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = ext_val, xend = start, yend = ext_val), colour = "black", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = ext_val*2, xend = start, yend = ext_val*2), colour = "black", alpha=1, size=0.3, inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = ext_val*3, xend = start, yend = ext_val*3), colour = "black", alpha=1, size=0.3, inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = ext_val*4, xend = start, yend = ext_val*4), colour = "black", alpha=1, size=0.3, inherit.aes = FALSE ) +
    
    # Add text showing the value of each 100/75/50/25 lines
    ggplot2::annotate("text", x = rep(max(data$id),5), y = c(ext_val/3, ext_val + ext_val/3, ext_val*2 + ext_val/3, ext_val*3 + ext_val/3, ext_val*4 + ext_val/3), label = lab , color="black", size = 2 , angle=6.5, hjust=1) +
    
    ylim(-ext_val*3.2, ext_val*4*4/3) +
    
    # Add labels on top of each bar
    geom_text(data = label_data, aes(x = id, y = tot + 100, label=feature, hjust=hjust), color="black", alpha=0.6, size=2.3, angle = label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data = base_data, aes(x = start, y = -ext_val/7.5, xend = end, yend = -ext_val/7.5), colour = "black", alpha = 0.8, size = 0.5 , inherit.aes = FALSE )  +
    geom_text(data = base_data, aes(x = title, y = -ext_val/1.875, label = group), hjust = c(1,1,0,0), colour = "black", alpha = 0.8, size = 2, fontface = "bold", inherit.aes = FALSE) + 
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    coord_polar()
  
  return(p)
}

