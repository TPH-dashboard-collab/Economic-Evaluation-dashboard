###Function to calculate optimal allocation, to speed calculations up, 
#' this can be done by restricted to Pareto frontier policies 
#' Optimal allocation of interventions to each geographic unit with a given
#' budget envelope
#' 
#' @param df data.frame or data.table containing at least four columns with 
#' hard-coded column names: 1) 'region' for geographic unit, 2) 'policy' for 
#' intervention combination in particular geographic unit, 3) cost_name for
#' cost data used (see argument cost_name argument), 4) value_var for health impact (see
#' value_var argument)
#' @param budget_env Total budget envelope available
#' @param region_name Column name for the geographic unit.
#' @param policy_name Column name for the intervention scenario.
#' @param cost_name name for the cost of the intervention scenario in df
#' @param health_name name for the health outcome of the intervention in df
#' scenario.
#' @param optim_dir Should we maximize ('max') or minimize ('min') values for
#' health_name 
#' should change sign (value_sign=-1) or not (value_sign=1)
#' @param keep_col Character vector with column names in df besides region_name, policy_name,
#' health_name, cost_name are to kept in output, 
#' if NULL all columns are kept

#' @returns The input data.table df with an additional columns:
#' 1) 'policy_allocation' value between 0 and 1, fraction of allocation for
#' particular policy, this should be 0 or 1 for all region, except for one region;
#' if you filter df on policy_allocation>0, you will obtain all relevant for the
#' optimal allocation
#' 2) 'optimal_{health_name}' health impact of optimal solution
#' 3) 'optimal_{cost_name}' cost impact of optimal solution
#' 4) 'budget_env' the value provided for the budget_env argument
#' @examples
OptimalAllocation<-function(df,
                            budget_env=2*10^6,
                            region_name="setting",
                            policy_name="scenario_name",
                            cost_name="tot.mean_total_int_cost",
                            health_name="ca.tot.mean_tUncomp",
                            optim_dir="max",
                            keep_col=NULL){
  
  if (!optim_dir %in% c("max", "min")) {
    stop("'optim_dir' must be either 'max' or 'min'.", call. = FALSE)
  }
  message(paste0("Value to be optimized: ",health_name))
  message(paste0("Optimization direction for ",health_name,": ", optim_dir))
  message(paste0("Cost associated with ",health_name,": ",cost_name))
  message(paste0("Cost envelope: ",budget_env))
  
  setDT(df)
  if (!is.null(keep_col)&!all(keep_col %in% names(df))) {
    stop("all 'keep_col' must  be  colnumn names of df", call. = FALSE)
  } else if (!is.null(keep_col)) {
    keep_col<-c(region_name,policy_name,health_name,cost_name,keep_col)
    df <- df[,..keep_col]
  }
  
  setnames(df, old = c(region_name, policy_name),
           new = c("region", "policy"))
  
  min_expenditure <- df[, .SD[which.min(get(cost_name))], by = "region"][,round(sum(get(cost_name)))]
  message(paste0("Minimal expenditure across all plans: ",min_expenditure))
  if(min_expenditure>budget_env){
    warning("budget_env is less than minimally attainable cost!")
  }
  
  df[, `:=`(
    N_policies = .N
  ), by = region]
  
  df[, `:=`(
    N_regions = uniqueN(region)
  )]
  
  
  df_idx <- unique(df[, .(region, N_policies)])[
    , `:=`(
      start = cumsum(N_policies) - N_policies + 1,
      stop  = cumsum(N_policies)
    )
  ]
  
  objective_coeffs <- df[[health_name]] ##maximize total health gain
  
  ###constraint matrix
  
  ##budget constraint
  matrix(df[[cost_name]],
         nrow=1,byrow=T)->constr_budget_env
  ##in each region, policies are mutually exclusive, block design
  matrix(rep(0,nrow(df)*unique(df$N_regions)),
         nrow=unique(df$N_regions),byrow=T)->constr_per_region
  for(i in 1:nrow(constr_per_region))
  {
    constr_per_region[i,c(df_idx$start[i]:df_idx$stop[i])]<-1
  }
  
  ## policy cannot be implemented less than not at all
  diag(rep(1,nrow(df)))->constr_per_region_policy
  
  constraint_matrix <- rbind(constr_budget_env,
                             constr_per_region,
                             constr_per_region_policy)
  ##right-hand side
  rhs <- c(budget_env,
           rep(1,nrow(constr_per_region)),
           rep(0,nrow(constr_per_region_policy)))
  constraint_dir <- c("<=",
                      rep("<=",nrow(constr_per_region)),
                      rep(">=",nrow(constr_per_region_policy)))
  
  
  # Solve the linear program
  solution <- lpSolve::lp(direction = optim_dir,        # "max" for maximization
                          objective.in = objective_coeffs,  # Objective coefficients
                          const.mat = constraint_matrix,    # Constraint matrix
                          const.dir = constraint_dir,       # Constraint directions
                          const.rhs = rhs)                  # Right-hand side
  
  opt_health_col<-paste0("optimal_",health_name)
  opt_cost_col<-paste0("optimal_",cost_name)
  
  result <- df[
    ,
    `:=`(
      policy_allocation = solution$solution,
      opt_health_col = solution$solution * objective_coeffs,
      opt_cost_col = solution$solution * constr_budget_env[1,],
      budget_env = budget_env
    )
  ]
  
  return(result)
}

