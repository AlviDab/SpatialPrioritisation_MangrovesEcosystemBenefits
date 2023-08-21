# Authors: T. Buenafe and A. Dabalà
# Updated: 05-03-2022

# Purpose: Create a Cohen's Kappa correlation plot
# Inputs:
# 1. sol: List of prioritizr solutions (sf) with solutions having a column name `solution_1`
# 2. name_sol: Name tags to the different solutions
# Outputs
# object with the following list of information
# object$plot() : shows the plot in RStudio
# object$matrix : shows the matrix

fcreate_kappacorrplot <- function(sol, name_sol) {
  
  pacman::p_load(irr, sf, tidyverse, corrplot)
  
  s_list <- lapply(seq_along(sol), function(x) {
    sol[[x]] %>% 
      tibble::as_tibble() %>% 
      dplyr::select(solution_1) %>% 
      setNames(name_sol[[x]])
  }
  )
  
  y = 1
  s_matrix <- list()
  for(i in 1:length(s_list)) {
    for(j in 1:length(s_list)) {
      kappa_temp <- irr::kappa2(bind_cols(s_list[[i]], s_list[[j]]))
      kappa_corrvalue <- kappa_temp$value
      kappa_pvalue <- kappa_temp$p.value
      s_matrix[[y]] <- cbind(colnames(s_list[[i]]), colnames(s_list[[j]]), kappa_corrvalue, kappa_pvalue)
      y = y + 1
    }
  }
  
  s_matrix_all <- do.call(rbind, s_matrix) %>% 
    tibble::as_tibble()
  colnames(s_matrix_all)[1:2] <- c('plan1','plan2')
  
  matrix_final <- s_matrix_all %>% 
    tibble::as_tibble() %>% 
    dplyr::select(-kappa_pvalue) %>% 
    tidyr::pivot_wider(names_from = plan2, values_from = kappa_corrvalue) %>% 
    as.matrix()
  
  matrix_x <- s_matrix_all %>% 
    as_tibble()
  
  # creating corrplot
  rownames(matrix_final) <- matrix_final[,1]
  n <- length(s_list) + 1 # 4 is the number of inputted scenarios
  matrix <- matrix_final[,2:n]
  class(matrix) <- "numeric"
  
  plot <- function() {
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    corrplot(matrix, method = "shade", tl.col = "black", addCoef.col = "black",
             col=col(200), tl.srt=45)
    recordPlot()
  }
  
  object <- list(matrix = matrix, plot = plot)
  return(object)
}
