
# utils.R
# Malaria Economic Evaluation Dashboard — Tanzania

# Thes are pure helper functions used by my s server.R.
# All functions are stateless (no Shiny reactives or globals).
# Sources:
#   - data.table for all tabular operations
#   - lpSolve for budget optimisation
#   - ggplot2 for tornado charts
#   - sf / leaflet for map rendering




# THE OPTIMISATION FUNCTION 

#' Solve a budget-constrained resource allocation problem using linear programming
#'
#' @description
#' Given a set of districts (regions) and candidate intervention scenarios
#' (policies) with associated costs and health outcomes, this function selects
#' exactly one scenario per district that maximises (or minimises) the total
#' health objective while keeping total cost within the budget envelope.
#'
#' The LP formulation:
#'   - Binary decision variable x_{r,p} = 1 if policy p is chosen for region r
#'   - Objective : max/min  sum_{r,p} health_{r,p} * x_{r,p}
#'   - Budget    : sum_{r,p} cost_{r,p}  * x_{r,p}  <= budget_env
#'   - Uniqueness: sum_p x_{r,p} = 1  for each region r
#'
#' @param df          data.frame or data.table with one row per region-policy pair.
#'                    Must contain columns named by region_name, policy_name,
#'                    cost_name, and health_name.
#' @param budget_env  Numeric scalar. Total budget available across all regions (USD).
#' @param region_name Character. Column name identifying the geographic unit
#'                    (default "admin_2").
#' @param policy_name Character. Column name identifying the intervention scenario
#'                    (default "scenario_name").
#' @param cost_name   Character. Column name for the cost of each scenario per region
#'                    (default "avg_cost").
#' @param health_name Character. Column name for the health metric to optimise
#'                    (default "averted"). Must be numeric.
#' @param optim_dir   Character. Direction of optimisation: "max" (default) or "min".
#'
#' @return A data.table containing the selected rows from df (one per region),
#'         with original columns intact plus `policy_allocation` (≈ 1 for selected rows).
#'         Returns an empty data.table if no feasible solution exists.
#'
#' @examples
#' \dontrun{
#'   result <- OptimalAllocation(
#'     df          = metrics_mean,
#'     budget_env  = 71000000,
#'     health_name = "averted"
#'   )
#' }
OptimalAllocation <- function(df,
                              budget_env,
                              region_name  = "admin_2",
                              policy_name  = "scenario_name",
                              cost_name    = "avg_cost",
                              health_name  = "averted",
                              optim_dir    = "max") {
  
  # Work on an internal copy to avoid modifying the caller's data
  working_df <- copy(as.data.table(df))
  setnames(working_df,
           old = c(region_name, policy_name),
           new = c("region",    "policy"))
  
  # Count policies per region and number of distinct regions
  working_df[, N_policies := .N,        by = region]
  working_df[, N_regions  := uniqueN(region)]
  
  # Build index table: start/stop row positions for each region in the LP matrix
  df_idx <- unique(working_df[, .(region, N_policies)])[
    , `:=`(
      start = cumsum(N_policies) - N_policies + 1L,
      stop  = cumsum(N_policies)
    )]
  
  # LP objective: health metric for every region-policy combination
  objective_coeffs <- working_df[[health_name]]
  
  # Budget constraint row: cost of each region-policy combination
  constr_budget <- matrix(working_df[[cost_name]], nrow = 1, byrow = TRUE)
  
  # Uniqueness constraints: one row per region, 1s spanning that region's policies
  n_regions  <- unique(working_df$N_regions)
  n_combos   <- nrow(working_df)
  constr_one <- matrix(0L, nrow = n_regions, ncol = n_combos)
  for (i in seq_len(n_regions)) {
    constr_one[i, df_idx$start[i]:df_idx$stop[i]] <- 1L
  }
  
  # Solve the LP
  solution <- lpSolve::lp(
    direction    = optim_dir,
    objective.in = objective_coeffs,
    const.mat    = rbind(constr_budget, constr_one),
    const.dir    = c("<=", rep("=", n_regions)),
    const.rhs    = c(budget_env, rep(1L, n_regions))
  )
  
  # Attach allocation flag and return selected rows only
  working_df[, policy_allocation := solution$solution]
  return(working_df[policy_allocation > 0.99])
}


#  DISTRICT CLASSIFICATION FUNCTION  

#' Classify an optimal district allocation relative to the reference plan
#'
#' @description
#' Compares the number of active interventions in the optimal scenario against
#' the reference scenario and assigns one of four policy labels:
#' \itemize{
#'   \item \strong{Reference}   — optimizer chose the same scenario as the reference
#'   \item \strong{Added}       — optimal has more interventions than reference
#'   \item \strong{Reduced}     — optimal has fewer interventions than reference
#'   \item \strong{Substituted} — optimal has a different mix at similar count
#' }
#'
#' @param opt_scenario Character. scenario_name selected by the LP optimizer.
#' @param ref_scenario Character. scenario_name of the reference plan (e.g. "bau").
#' @param scen_int_counts data.table with columns `scenario_name` (character) and
#'        `n_ints` (integer). Pre-computed at startup; passed in to keep the
#'        function stateless (no global look-up inside the function body).
#'
#' @return Character scalar: one of "Reference", "Added", "Reduced", "Substituted".
#'
#' @examples
#' \dontrun{
#'   classify_district("ICCM_SMC", "bau", scen_int_counts)
#'   # Returns "Added" if ICCM_SMC has more interventions than bau
#' }
classify_district <- function(opt_scenario, ref_scenario, scen_int_counts) {
  
  # Guard against NA or missing values passed from mapply
  if (is.na(opt_scenario) || length(opt_scenario) == 0L) return("Reference")
  if (is.na(ref_scenario) || length(ref_scenario) == 0L) return("Reference")
  
  # Exact match → no change from reference
  if (opt_scenario == ref_scenario) return("Reference")
  
  opt_n <- scen_int_counts[scenario_name == opt_scenario, n_ints][1]
  ref_n <- scen_int_counts[scenario_name == ref_scenario, n_ints][1]
  
  # Guard against missing entries in the lookup table
  if (is.na(opt_n) || is.na(ref_n)) return("Substituted")
  
  if (opt_n > ref_n) return("Added")
  if (opt_n < ref_n) return("Reduced")
  return("Substituted")
}


#  METRICS COMPUTATION FUNCTION 

#' Compute per-scenario health and economic metrics across all districts
#'
#' @description
#' For a given set of user inputs (years, age group, unit costs, WTP, reference
#' plan) this function:
#' \enumerate{
#'   \item Calculates the total intervention cost per scenario-district-EIR_CI
#'         combination by multiplying nHost × active × coverage × unit_cost.
#'   \item Computes cumulative cases occurring (\code{averted_period}) as the
#'         value of \code{cum_nUncomp} at the final year of the selected period.
#'         Because the simulation starts at zero, this equals total period cases.
#'   \item Merges in reference-plan values to derive incremental metrics:
#'         \code{averted}, \code{cost_diff}, \code{NMB}, \code{ICER}, \code{is_CE}.
#' }
#'
#' @param data_tza1   data.table. Full simulation dataset (raw, unfiltered).
#' @param int_names   Character vector. Intervention names derived from
#'                    \code{active_int_*} column stubs.
#' @param u_costs     Named list of numeric unit costs, one entry per intervention
#'                    name (same names as \code{int_names}).
#' @param year_start  Integer. First year of the evaluation period.
#' @param year_end    Integer. Last year of the evaluation period.
#' @param age_groups  Character vector. Age group(s) to include (e.g. "0-100").
#' @param ref_scen    Character. Lower-case scenario_name of the reference plan
#'                    (e.g. "bau").
#' @param wtp         Numeric. Willingness-to-pay threshold (USD per case averted).
#'
#' @return data.table with one row per scenario-district-EIR_CI combination and
#'         columns: scenario_name, plan, admin_2, EIR_CI, averted_period,
#'         avg_cost, r_c, r_cost, averted, cost_diff, NMB, is_CE, ICER.
#'         
#' @examples
compute_metrics <- function(data_tza1, int_names, u_costs,
                            year_start, year_end, age_groups,
                            ref_scen, wtp) {
  
  # Intervention costs 
  # Filter to the evaluation period and selected age groups
  dt <- data_tza1[year >= year_start & year <= year_end & age_group %in% age_groups]
  
  dt[, r_cost := 0]
  for (int in int_names) {
    act <- paste0("active_int_",   int)
    cov <- paste0("coverage_int_", int)
    # Only compute cost if both active flag and coverage columns exist
    if (act %in% names(dt) && cov %in% names(dt)) {
      dt[, r_cost := r_cost + (nHost * get(act) * get(cov) * u_costs[[int]])]
    }
  }
  
  # Cost: sum across age groups within each year, then average across years
  # Exactly preserves the original working logic
  costs <- dt[, .(avg_cost = mean(sum(r_cost))),
              by = .(scenario_name, plan, admin_2, EIR_CI)]
  
  # Cumulative cases occurring 
  # Original working logic: mean(cum_nUncomp) grouped by plan to match costs merge
  dt_end <- data_tza1[
    year == year_end & age_group %in% age_groups,
    .(c_e = mean(cum_nUncomp)),
    by = .(scenario_name, plan, admin_2, EIR_CI)]
  
  dt_st <- data_tza1[
    year == (year_start - 1L) & age_group %in% age_groups,
    .(c_s = mean(cum_nUncomp)),
    by = .(scenario_name, plan, admin_2, EIR_CI)]
  
  impact <- merge(dt_end, dt_st,
                  by  = c("scenario_name", "plan", "admin_2", "EIR_CI"),
                  all.x = TRUE)
  impact[, averted_period := c_e - fifelse(is.na(c_s), 0, c_s)]
  
  #  Merge costs and compute incremental metrics 
  m <- merge(impact, costs, by = c("scenario_name", "plan", "admin_2", "EIR_CI"))
  
  # Reference row: cases and costs for the chosen reference scenario
  ref <- m[tolower(scenario_name) == ref_scen,
           .(admin_2, EIR_CI, r_c = averted_period, r_cost = avg_cost)]
  
  m <- merge(m, ref, by = c("admin_2", "EIR_CI"), all.x = TRUE)
  
  # Incremental health and cost vs reference
  m[, `:=`(
    averted   = r_c - averted_period,   # cases prevented vs reference (higher = better)
    cost_diff = avg_cost - r_cost        # incremental cost vs reference (positive = costlier)
  )]
  
  # Economic evaluation metrics
  m[, `:=`(
    NMB   = (averted * wtp) - cost_diff,        # net monetary benefit
    is_CE = ((averted * wtp) - cost_diff) >= 0  # cost-effective at this WTP?
  )]
  m[, ICER := cost_diff / averted]              # incremental cost-effectiveness ratio
  
  return(m)
}


#' Compute reference-plan budget anchored to BAU cost
#'
#' @description
#' The budget envelope is always anchored to the BAU (Business As Usual) cost,
#' regardless of which reference plan is selected for the health comparison.
#' This ensures a fair comparison: switching the reference plan from BAU to NSP
#' does not inflate the optimizer's budget.
#'
#' @param metrics    data.table. Output of \code{compute_metrics()}.
#' @param budget_adj Numeric. Percentage adjustment to apply to the BAU budget
#'                   (e.g. 10 means +10\%, -5 means -5\%).
#'
#' @return Named list with:
#' \describe{
#'   \item{curr}{Numeric. BAU base budget (USD).}
#'   \item{env}{Numeric. Adjusted budget envelope = curr × (1 + budget_adj/100).}
#'   \item{adj_pct}{Numeric. The percentage adjustment supplied.}
#' }
#' 
#' @examples
compute_budget <- function(metrics, budget_adj, ref_scen) {
  
  # Budget anchored to BAU cost regardless of selected reference plan.
  # This ensures NSP and BAU optimizer always compete on equal footing.
  # ref_scen passed explicitly so this function remains stateless.
  bau_rows <- metrics[tolower(scenario_name) == "bau" & EIR_CI == "EIR_mean"]
  
  if (nrow(bau_rows) == 0L) {
    stop("compute_budget: no BAU rows found. ",
         "Check scenario_name 'bau' exists and EIR_CI contains 'EIR_mean'.")
  }
  
  bau_cost <- bau_rows[, sum(avg_cost, na.rm = TRUE)]
  
  list(
    curr    = bau_cost,
    env     = bau_cost * (1 + budget_adj / 100),
    adj_pct = budget_adj
  )
}


#  MAP HELPERS 

#' Join district-level data to the Tanzania shapefile for mapping
#'
#' @description
#' Performs a left join between the shapefile and a data subset, matching on
#' an upper-cased, trimmed version of the \code{admin_2} district name.
#' Returns an sf object suitable for use in \code{leaflet()} or \code{ggplot()}.
#'
#' @param data_subset data.table or data.frame containing at minimum an
#'                    \code{admin_2} column.
#' @param shape_file_tza sf object. The Tanzania district shapefile, already
#'                       transformed to WGS84 and containing a \code{join_id}
#'                       column (upper-cased admin_2).
#'
#' @return sf object (same CRS as shape_file_tza) with all columns from both
#'         the shapefile and \code{data_subset}.
#'         
#' @examples

build_map_obj <- function(data_subset, shape_file_tza) {
  
  # Standardise join key: upper-case, trimmed district name
  data_clean <- as.data.table(data_subset)
  data_clean[, join_id := toupper(trimws(as.character(admin_2)))]
  
  # Join using data.table and immediately wrap in st_as_sf to preserve geometry
  # This performs a left join (keeping all rows from shape_file_tza)
  merged_data <- data_clean[as.data.table(shape_file_tza), on = "join_id"]
  
  sf::st_as_sf(merged_data)
}

#' Prepare long-format data for the facet intervention map
#'
#' @description
#' Takes the LP-selected scenarios and the raw data, derives intervention group
#' status for each district (CM & ICCM, Nets, PMC & SMC, LSM, Vaccine,
#' IPTSc, IRS), and returns a long-format data.table with one row per
#' district × intervention group, excluding "None" rows.
#'
#' @param opt_choices data.table. Best LP allocation with columns \code{region}
#'                    (district) and \code{policy} (scenario_name).
#' @param data_tza1   data.table. Full raw dataset; used to look up intervention
#'                    active flags for the first year of simulation.
#' @param all_active_cols Character vector. Names of all \code{active_int_*}
#'                        columns in \code{data_tza1}.
#'
#' @return data.table with columns: admin_2, Group (intervention category),
#'         Status (e.g. "CM Only", "PBO Nets"). Rows with Status == "None"
#'         are excluded.
#'         
#' @examples
prepare_facet_data <- function(opt_choices, data_tza1, all_active_cols) {
  
  # Extract active intervention flags for the first simulation year
  scen_info <- data_tza1[
    year == min(data_tza1$year) & age_group == "0-100",
    .SD, .SDcols = c("admin_2", "scenario_name", all_active_cols)]
  
  # Merge LP selections with intervention flags
  plot_dt <- merge(
    opt_choices[, .(admin_2 = region, scenario_name = policy)],
    scen_info,
    by = c("admin_2", "scenario_name"),
    all.x = TRUE
  )
  
  # Derive intervention group status labels
  plot_dt[, `CM & ICCM` := fcase(
    active_int_CM == 1L & active_int_ICCM == 1L, "CM + iCCM",
    active_int_CM == 1L,                          "CM Only",
    active_int_ICCM == 1L,                        "iCCM Only",
    default = "None"
  )]
  
  plot_dt[, `PMC & SMC` := fcase(
    active_int_PMC == 1L & active_int_SMC == 1L, "PMC + SMC",
    active_int_PMC == 1L,                         "PMC Only",
    active_int_SMC == 1L,                         "SMC Only",
    default = "None"
  )]
  
  plot_dt[, Nets := fcase(
    (active_int_IG2_Nets + active_int_PBO_Nets + active_int_STD_Nets) > 1L, "Multiple Nets",
    active_int_IG2_Nets == 1L,                                               "IG2 Nets",
    active_int_PBO_Nets == 1L,                                               "PBO Nets",
    active_int_STD_Nets == 1L,                                               "STD Nets",
    default = "None"
  )]
  
  plot_dt[, LSM     := fifelse(active_int_LSM == 1L,     "LSM",     "None")]
  plot_dt[, Vaccine := fifelse(active_int_Vaccine == 1L, "Vaccine", "None")]
  plot_dt[, IPTSc   := fifelse(active_int_IPTSc == 1L,   "IPTSc",   "None")]
  plot_dt[, IRS     := fifelse(active_int_IRS == 1L,     "IRS",     "None")]
  
  # Pivot to long format: one row per district × intervention group
  groups <- c("CM & ICCM", "PMC & SMC", "Nets", "LSM", "Vaccine", "IPTSc", "IRS")
  keep   <- c("admin_2", groups)
  long   <- melt(
    plot_dt[, ..keep],
    id.vars       = "admin_2",
    variable.name = "Group",
    value.name    = "Status"
  )
  
  # Drop "None" rows — only districts with active interventions appear on map
  long[Status != "None"]
}


# PLOTTING

#' Build a dual-axis tornado chart of cost vs cases averted
#'
#' @description
#' For a given LP result, groups districts by their selected intervention
#' combination and draws a horizontal bar chart with:
#' \itemize{
#'   \item \strong{Left (gold)} bars: total plan cost in M USD (scaled to share
#'         the x-axis with the right-side bars)
#'   \item \strong{Right (purple)} bars: total cases averted vs reference
#' }
#' Rows are sorted by cases averted ascending so the best-performing combination
#' appears at the top. Negative bars indicate the optimizer selected a scenario
#' worse than the reference in health terms (can occur when optimising on NMB
#' with a low WTP threshold).
#'
#' @param opt_res     data.table. LP result from \code{OptimalAllocation()},
#'                    containing columns: region, policy, averted, avg_cost.
#' @param scen_lookup data.table. Pre-computed scenario summary lookup with
#'                    columns: scenario_name, active_summary. Used to label
#'                    rows by intervention combination.
#'
#' @return A \code{ggplot} object. Render with \code{print()} or inside
#'         \code{renderPlot()}.
#'         
#' @examples
make_tornado <- function(opt_res, scen_lookup) {
  
  # Build district-level data from LP result (no additional merges needed)
  plot_dt <- data.table(
    admin_2       = opt_res$region,
    scenario_name = opt_res$policy,
    cases_averted = opt_res$averted,       # r_c - averted_period per district
    avg_cost      = opt_res$avg_cost       # total cost for selected scenario
  )
  
  # Attach human-readable intervention combination label
  plot_dt <- merge(
    plot_dt,
    unique(scen_lookup[, .(scenario_name, active_summary)]),
    by    = "scenario_name",
    all.x = TRUE
  )
  plot_dt[, combo := fifelse(
    is.na(active_summary) | active_summary == "No additional interventions",
    "Reference only",
    active_summary
  )]
  
  # Aggregate across districts sharing the same intervention combination
  tornado_dt <- plot_dt[, .(
    cases_averted = sum(cases_averted, na.rm = TRUE),
    cost_M        = sum(avg_cost,      na.rm = TRUE) / 1e6
  ), by = combo]
  
  # Sort ascending so highest-performing combo appears at top of chart
  tornado_dt <- tornado_dt[order(cases_averted)]
  tornado_dt[, combo := factor(combo, levels = unique(combo))]
  
  # Scale factor: maps cost (M USD) onto the same x-axis range as cases averted
  max_cases <- max(abs(tornado_dt$cases_averted), na.rm = TRUE)
  max_cost  <- max(abs(tornado_dt$cost_M),        na.rm = TRUE)
  k <- ifelse(max_cost > 0, max_cases / max_cost, 1)
  
  # Negative cost_scaled pushes gold bars to the left of the zero line
  tornado_dt[, cost_scaled := -cost_M * k]
  
  # Smart label formatter: K / M suffixes
  fmt_cases <- function(x) {
    ifelse(abs(x) >= 1e6,
           paste0(round(x / 1e6, 2), "M"),
           ifelse(abs(x) >= 1e3,
                  paste0(round(x / 1e3, 1), "K"),
                  format(round(x), big.mark = ",")))
  }
  
  ggplot(tornado_dt, aes(y = combo)) +
    # Gold bars — cost, extending left from zero
    geom_col(aes(x = cost_scaled),   fill = "#E8A020", width = 0.55) +
    # Purple bars — cases averted, extending right from zero
    geom_col(aes(x = cases_averted), fill = "#4B0082", width = 0.55) +
    # Cost labels just outside left bars
    geom_text(aes(x = cost_scaled,
                  label = paste0("$", round(cost_M, 1), "M")),
              hjust = 1.08, size = 3.3, color = "gray20") +
    # Cases averted labels just outside right bars (flip side for negatives)
    geom_text(aes(x     = cases_averted,
                  label = fmt_cases(cases_averted),
                  hjust = ifelse(cases_averted >= 0, -0.08, 1.08)),
              size = 3.3, color = "gray20") +
    geom_vline(xintercept = 0, color = "gray40", linewidth = 0.6) +
    scale_x_continuous(
      labels  = function(x) {
        ifelse(x >= 0,
               ifelse(x >= 1e6,
                      paste0(round(x / 1e6, 1), "M"),
                      paste0(round(x / 1e3), "K")),
               paste0("$", round(abs(x) / k, 1), "M"))
      },
      expand = expansion(mult = c(0.28, 0.2))
    ) +
    labs(
      x       = NULL,
      y       = NULL,
      caption = "\u25A0 Purple = Cases Averted (right)     \u25A0 Gold = Cost in M USD (left)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.y        = element_text(size = 9),
      axis.text.x        = element_text(size = 9),
      plot.caption       = element_text(size = 10, hjust = 0.5, color = "gray30"),
      plot.margin        = margin(8, 45, 8, 8)
    )
}


#' Build the ggplot facet map of selected intervention groups
#'
#' @description
#' Takes the long-format district × intervention-group data produced by
#' \code{prepare_facet_data()} and renders a small-multiple map with one
#' facet per intervention group, coloured by intervention status.
#'
#' @param plot_dt_long data.table. Long-format output from \code{prepare_facet_data()},
#'                     with columns: admin_2, Group, Status.
#' @param shape_file_tza sf object. Tanzania district shapefile.
#' @param tza_outline    sf object. Tanzania national outline (for background).
#'
#' @return A \code{ggplot} object.
#' 
#' @examples
make_facet_map <- function(plot_dt_long, shape_file_tza, tza_outline) {
  
  # Colour palette: 14 distinct statuses across 7 intervention groups
  status_colors <- c(
    # CM & ICCM
    "CM Only"       = "#2196F3",
    "iCCM Only"     = "#00796B",
    "CM + iCCM"     = "#1A237E",
    # PMC & SMC
    "PMC Only"      = "#7B1FA2",
    "SMC Only"      = "#388E3C",
    "PMC + SMC"     = "#4A148C",
    # Nets
    "STD Nets"      = "#90CAF9",
    "PBO Nets"      = "#1565C0",
    "IG2 Nets"      = "#0D47A1",
    "Multiple Nets" = "#01579B",
    # Single interventions
    "LSM"           = "#F57F17",
    "Vaccine"       = "#C62828",
    "IPTSc"         = "#558B2F",
    "IRS"           = "#6D4C41"
  )
  
  map_obj <- build_map_obj(plot_dt_long, shape_file_tza)
  # Remove unmatched shapefile rows (districts with no active interventions)
  map_obj <- map_obj[!is.na(map_obj$Group), ]
  
  ggplot(map_obj) +
    geom_sf(data = tza_outline, fill = "#f2f2f2", color = "white", size = 0.05) +
    geom_sf(aes(fill = Status), color = NA) +
    facet_wrap(~Group, ncol = 3, drop = TRUE) +
    scale_fill_manual(values = status_colors,
                      name   = "Intervention Status",
                      drop   = FALSE) +
    theme_void() +
    theme(
      strip.text      = element_text(size = 13, face = "bold", margin = margin(b = 10)),
      legend.position = "bottom",
      legend.title    = element_text(face = "bold"),
      panel.spacing   = unit(2, "lines"),
      plot.margin     = margin(20, 20, 20, 20)
    ) +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE,
                               override.aes = list(size = 5)))
}


# SENSITIVITY ANALYSIS (run_sensitivity function)

#' Run budget sensitivity analysis across a fixed set of budget steps
#'
#' @description
#' For each budget step in \code{sens_steps} that is less than or equal to
#' \code{max_adj}, re-solves the LP at that budget level and records the total
#' cases averted. A monotonicity correction is applied: if increasing the
#' budget yields fewer cases averted (due to LP tie-breaking), the previous
#' step's value is retained.
#'
#' @param res_mean    data.table. EIR_mean-filtered metrics from
#'                    \code{compute_metrics()}.
#' @param base_budget Numeric. BAU budget (USD), i.e. \code{budget_metrics$curr}.
#' @param max_adj     Numeric. Maximum slider value (e.g. 20 means show steps
#'                    up to +20\%).
#' @param sens_steps  Numeric vector. Budget increment percentages to evaluate
#'                    (default \code{c(0, 5, 10, 15, 20, 25, 30)}).
#'
#' @return List of lists, one per active step, each containing:
#' \describe{
#'   \item{pct}{Numeric. Budget increment percentage.}
#'   \item{budget}{Numeric. Absolute budget at this step (USD).}
#'   \item{cases_averted}{Numeric. Total cases averted vs reference at this budget.}
#'   \item{opt_res}{data.table or NULL. LP result at this step.}
#' }
#' 
#' @examples
run_sensitivity <- function(res_mean, base_budget, max_adj,
                            sens_steps = seq(-10, 10, by = 2)) {
  
  # Show all steps from the most negative up to current slider value
  # Always include 0% as the baseline reference point
  active_steps <- sort(unique(c(0L, sens_steps[sens_steps <= max_adj])))
  
  results <- lapply(active_steps, function(pct) {
    
    bud <- base_budget * (1 + pct / 100)
    
    # Guard: budget must be positive
    if (bud <= 0) {
      return(list(pct = pct, budget = bud, cases_averted = 0, opt_res = NULL))
    }
    
    opt <- tryCatch(
      OptimalAllocation(
        df          = res_mean,
        budget_env  = bud,
        region_name = "admin_2",
        policy_name = "scenario_name",
        cost_name   = "avg_cost",
        health_name = "averted",
        optim_dir   = "max"
      ),
      error = function(e) NULL
    )
    
    if (is.null(opt) || nrow(opt) == 0L) {
      list(pct = pct, budget = bud, cases_averted = 0, opt_res = NULL)
    } else {
      list(pct           = pct,
           budget        = bud,
           cases_averted = sum(opt$averted, na.rm = TRUE),
           opt_res       = opt)
    }
  })
  
  #  Monotonicity(orderly increaments) corrections 
  # Get baseline (0%) cases averted
  baseline <- results[[which(sapply(results, `[[`, "pct") == 0)]]$cases_averted
  
  # Positive steps: cases averted must be non-decreasing as budget increases
  pos_idx <- which(sapply(results, `[[`, "pct") > 0)
  if (length(pos_idx) > 0L) {
    prev <- baseline
    for (i in pos_idx) {
      if (results[[i]]$cases_averted < prev) {
        results[[i]]$cases_averted <- prev
      }
      prev <- results[[i]]$cases_averted
    }
  }
  
  # Negative steps: cases averted must be non-increasing as budget decreases
  # i.e. cutting budget cannot increase cases averted vs baseline
  neg_idx <- rev(which(sapply(results, `[[`, "pct") < 0))  # from -2 down to most negative
  if (length(neg_idx) > 0L) {
    prev <- baseline
    for (i in neg_idx) {
      if (results[[i]]$cases_averted > prev) {
        results[[i]]$cases_averted <- prev
      }
      prev <- results[[i]]$cases_averted
    }
  }
  
  results
}


#' Build the sensitivity bar chart from run_sensitivity() output (make_sensitivity_bar function)
#'
#' @description
#' Renders a vertical bar chart with one bar per active budget step.
#' Each bar shows total cases averted; x-axis labels show both the
#' percentage increment and the absolute budget in M USD.
#'
#' @param results List. Output of \code{run_sensitivity()}.
#'
#' @return A \code{ggplot} object, or a blank plot with a message if
#'         \code{results} is empty.
#'         
#' @examples
make_sensitivity_bar <- function(results) {
  
  # Empty results — slider is at or below 0%
  if (length(results) == 0L) {
    return(
      ggplot() +
        geom_blank() +
        annotate("text", x = 0.5, y = 0.5, size = 5, color = "gray50",
                 label = "Move the Budget Change slider above 0% to reveal bars") +
        theme_void()
    )
  }
  
  # Smart label formatter
  fmt_cases <- function(x) {
    if (x >= 1e6)      paste0(round(x / 1e6, 2), "M")
    else if (x >= 1e3) paste0(round(x / 1e3, 1), "K")
    else               format(round(x), big.mark = ",")
  }
  
  df <- data.table(
    idx         = factor(seq_along(results)),
    pct         = sapply(results, `[[`, "pct"),
    budget      = sapply(results, `[[`, "budget"),
    cases       = sapply(results, `[[`, "cases_averted")
  )
  df[, cases_label := sapply(cases, fmt_cases)]
  df[, x_label     := paste0("+", pct, "%  |  $", round(budget / 1e6, 1), "M")]
  
  ggplot(df, aes(x = idx, y = cases)) +
    geom_col(fill = "#2980b9", color = "white", width = 0.45) +
    geom_text(aes(label = cases_label),
              vjust = -0.5, fontface = "bold", size = 4.5, color = "#1a252f") +
    scale_x_discrete(labels = df$x_label) +
    scale_y_continuous(
      labels = function(y) ifelse(
        y >= 1e6,
        paste0(round(y / 1e6, 1), "M"),
        paste0(round(y / 1e3, 0), "K")
      ),
      expand = expansion(mult = c(0.02, 0.22))
    ) +
    labs(
      x        = "Budget Step  (% increase vs BAU  |  total envelope)",
      y        = "Cases Averted vs Reference",
      title    = "Cases Averted at Each Budget Level",
      subtitle = paste0(nrow(df), " of 7 steps shown — move slider right to reveal more")
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title         = element_text(face = "bold", size = 14),
      plot.subtitle      = element_text(color = "gray50", size = 11),
      axis.title         = element_text(face = "bold", size = 12),
      axis.text.x        = element_text(size = 10, color = "gray30",
                                        margin = margin(t = 6)),
      axis.ticks.x       = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.margin        = margin(12, 20, 16, 20)
    )
}


#  PRE-COMPUTATION HELPERS (build_scen_lookup function)

#' Build the scenario intervention summary lookup table
#'
#' @description
#' Pre-computes a table mapping each (admin_2, scenario_name) pair to a
#' human-readable string listing the active interventions. Called once at
#' startup; result stored as a global and reused across reactives to avoid
#' redundant computation.
#'
#' @param data_tza1      data.table. Full simulation dataset.
#' @param all_active_cols Character vector. Names of \code{active_int_*} columns.
#' @param int_names       Character vector. Intervention name stubs.
#'
#' @return data.table with columns: admin_2, scenario_name, active_summary.
#' 
#' @examples
build_scen_lookup <- function(data_tza1, all_active_cols, int_names) {
  
  temp <- unique(data_tza1[
    year == min(data_tza1$year),
    .SD, .SDcols = c("admin_2", "scenario_name", all_active_cols)
  ])
  
  temp[, active_summary := apply(.SD, 1, function(row) {
    active <- int_names[which(row == 1L)]
    if (length(active) == 0L) return("No additional interventions")
    paste(active, collapse = ", ")
  }), .SDcols = all_active_cols]
  
  temp[, .(admin_2, scenario_name, active_summary)]
}


#' Build the per-scenario intervention count lookup table (build_scen_int_counts function)
#'
#' @description
#' Derives the number of active interventions for each unique scenario from
#' the \code{active_summary} string produced by \code{build_scen_lookup()}.
#' Used by \code{classify_district()} to determine whether the optimal plan
#' adds, reduces, or substitutes interventions relative to the reference.
#'
#' @param scen_lookup data.table. Output of \code{build_scen_lookup()}, with
#'                    columns: scenario_name, active_summary.
#'
#' @return data.table with columns: scenario_name, n_ints (integer).
#' 
#' @examples
build_scen_int_counts <- function(scen_lookup) {
  
  unique(scen_lookup[, .(scenario_name, active_summary)])[
    , .(n_ints = fifelse(
      active_summary == "No additional interventions",
      0L,
      lengths(regmatches(active_summary,
                         gregexpr(",", active_summary))) + 1L
    )),
    by = scenario_name
  ]
}








