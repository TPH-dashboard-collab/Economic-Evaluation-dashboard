#' Calculate Pareto dominated intervention scenarios within a geographic unit.
#' 
#' @param df data.frame or data.table containing at least four columns 
#' (names of geographic unit and intervention scenario, numerical values of 
#' cost and health)
#' @param region_name Column name for the geographic unit.
#' @param policy_name Column name for the intervention scenario.
#' @param cost_name Column name for the cost of the intervention scenario.
#' @param health_name Column name for the health outcome of the intervention 
#' scenario.
#' @param health_sign Integer value indicating whether health outcome 
#' should change sign (health_sign=-1) or not (health_sign=1)

#' @return A list with 'result' which is the input data table df with 
#' an additional logical column called 
#' "dominated", TRUE means that the policy is dominated by at least
#' one other policy in the region. A policy is on the Pareto front, if it is 
#' not dominated, see column 'ParetoFrontier'. 
#' The logical column "convex_envelope" indicates whether strategies
#' are on the lower convex envelope. A policy is on the cost effectiveness frontier
#' if it is not dominatd and on the lower convex envelope, see column 
#' 'CostEffectivenessFrontier'.
#' Strings used for region_var and policy_var input are used
#' to rename column names in this data table.
#' If show_plot=T, then the return list also contains a second element 'plot'
#' @examples
Pareto_front<-function(df,
                       region_name="setting",
                       policy_name="scenario_name",
                       cost_name="ca.tot.mean_total_int_cost",
                       health_name="ca.tot.mean_tUncomp",
                       health_sign= 1,
		       show_plot=F){
	if (!health_sign %in% c(-1, 1)) {
		stop("'health_sign' must be either -1 or 1.", call. = FALSE)
	}
  data.table::setDT(df)# convert to data.table if not already
  data.table::setnames(df, c(region_name, policy_name), c("region", "policy"))
  
  df[, dC := get(cost_name)]
  df[, dI := health_sign*get(health_name)]
  
  result <- df[, {
    # For each (region), calculate domination per policy
    res <- lapply(.SD[, unique(policy)], function(p) {
      this_dC <- dC[policy == p]
      this_dI <- dI[policy == p]
      # Check if any other row dominates policy p
      dominated_flag <- any(dC <= this_dC & dI > this_dI)
      # Return all rows with that policy with dominated flag
      .SD[policy == p][, copy(.SD)][, dominated := dominated_flag]
    }) |> rbindlist()

    # --- Convex hull (lower part) ---
    res[, convex_envelope := FALSE]
    if (.N >= 3) {
      hull_idx <- grDevices::chull(res[, .(dC, dI)])
      hull_pts <- res[hull_idx][order(dC)]
      
      # find lower envelope: moving from left to right (increasing cost)
      # keep points that form a concave-up (lower) shape
      lower <- hull_pts[c(TRUE, diff(dI) / diff(dC) >= 0)]
      res[policy %in% lower$policy, convex_envelope := TRUE]
    } else {
      res[, convex_envelope := TRUE]
    }

  }, by = .(region)]
 
  result[,ParetoFrontier:=!dominated]
  result[,CostEffectivenessFrontier:=!dominated&convex_envelope]

  out<-list(result=NULL,show_plot=NULL)
  if(show_plot){
	  library(ggplot2)
	  library(ggrepel)

      ggplot(result)+
        geom_point(aes(x=dI,y=dC,color=ParetoFrontier))+
        geom_line(data = result%>%filter(CostEffectivenessFrontier),
                aes(x=dI,y=dC))+
        geom_text_repel(data = result%>%filter(CostEffectivenessFrontier),
                      aes(x=dI,y=dC,label=policy),
		      box.padding = 0.5,
		      point.padding = 0.5,
		      segment.color = "grey50",
		      segment.size = 0.5,
		      segment.alpha = 0.7)+
	scale_x_continuous(name=health_name)+
	scale_y_continuous(name=cost_name)+
        theme_bw()->out$show_plot
  }
  out$result<-result

  return(out)
}

