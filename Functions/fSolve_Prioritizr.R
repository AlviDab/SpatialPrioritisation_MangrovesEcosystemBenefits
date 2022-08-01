#Alvise Dabalà
#2022-03-20

#Function to produce and solve the prioritizr problem

fProblem_Prioritizr <- function(cost_col, value_penalty, linear_penalty, WDPA = FALSE, cons_feature, p_gap = 0.1) {
  if (WDPA == FALSE) {
    if(missing(value_penalty)) {
      p_ConsFeat <- problem(PUs, features = cons_feature$names, cost_column = cost_col) %>% #Area Target
        add_min_set_objective() %>%
        add_relative_targets(cons_feature$amount) %>% # representation targets (Area)
        add_binary_decisions() %>%
        add_gurobi_solver(verbose = FALSE, gap = p_gap)
      } else {
        p_ConsFeat <- problem(PUs, features = cons_feature$names, cost_column = cost_col) %>% #Area Target
          add_min_set_objective() %>%
          add_relative_targets(cons_feature$amount) %>% # representation targets (Area)
          add_binary_decisions() %>%
          add_linear_penalties(penalty = value_penalty, data = linear_penalty) %>% 
          add_gurobi_solver(verbose = FALSE, gap = p_gap)
      }
  } else {
    if(missing(value_penalty)) {
      p_ConsFeat <- problem(PUs, features = cons_feature$names, cost_column = cost_col) %>% #Area Target
        add_min_set_objective() %>%
        add_relative_targets(cons_feature$amount) %>% # representation targets (Area)
        add_locked_in_constraints(locked_in = "Protected") %>% 
        add_binary_decisions() %>%
        add_gurobi_solver(verbose = FALSE, gap = p_gap)
    } else {
      p_ConsFeat <- problem(PUs, features = cons_feature$names, cost_column = cost_col) %>% #Area Target
        add_min_set_objective() %>%
        add_relative_targets(cons_feature$amount) %>% # representation targets (Area)
        add_binary_decisions() %>%
        add_locked_in_constraints(locked_in = "Protected") %>% 
        add_linear_penalties(penalty = value_penalty, data = linear_penalty) %>% 
        add_gurobi_solver(verbose = FALSE, gap = p_gap)
    }
  }
}
