library(shiny)
library(tidyverse)
library(data.table)
library(sf)
library(leaflet)
library(lpSolve)


# --- SETTINGS ---
sub_level_col <- "admin_2"   # Column in CSV
shp_district_col <- "admin_2" # Column in Shapefile



data_tza <- fread("Economic-Evaluation-dashboard/tza_sample_data.csv") 

# Loading my Shaplsefile and Outline
shape_file_tza <- st_read("Economic-Evaluation-dashboard/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp") %>% 
  st_transform(4326) %>%
  rename(JOIN_TARGET = !!sym(shp_district_col)) # Renames the variable column to a fixed name

# 3. Create country outline
tza_outline <- st_union(shape_file_tza)


# Find all 11 interventions automatically
all_active_cols <- names(data_tza)[grep("^active_int_", names(data_tza))]
int_names <- gsub("active_int_", "", all_active_cols)

# --- MOCK ELIGIBILITY (Per Supervisor Instructions) ---
set.seed(123)
unique_subs <- unique(data_tza[[sub_level_col]])
for(int in int_names) {
  elig_col <- paste0("eligible_int_", int)
  lookup <- data.frame(s = unique_subs, v = sample(c(TRUE, FALSE), length(unique_subs), replace = TRUE))
  setnames(lookup, c("s", "v"), c(sub_level_col, elig_col))
  data_tza <- merge(data_tza, lookup, by = sub_level_col, all.x = TRUE)
}


# Incremental Impact (End Year - (Start Year - 1))

#  Adding 'plan' to the grouping
calc_district_impact <- function(data, start_yr, end_yr) {
  dt <- as.data.table(data)
  
  # Get End Year Cumulative - include 'plan' in 'by'
  dt_end <- dt[year == end_yr, .(
    c_end = mean(cum_nUncomp),
    d_end = mean(cum_expectedDirectDeaths)
  ), by = .(scenario_name, plan, admin_2, EIR_CI)] # <--- Added plan here
  
  # Get Start Year-1 Cumulative - include 'plan' in 'by'
  dt_start <- dt[year == (start_yr - 1), .(
    c_start = mean(cum_nUncomp),
    d_start = mean(cum_expectedDirectDeaths)
  ), by = .(scenario_name, plan, admin_2, EIR_CI)] # <--- Added plan here
  
  # Joined on plan as well to be safe
  res <- merge(dt_end, dt_start, by = c("scenario_name", "plan", "admin_2", "EIR_CI"), all.x = TRUE)
  res[is.na(c_start), c_start := 0]
  
  res[, `:=`(
    cases_in_period = c_end - c_start,
    deaths_in_period = d_end - d_start
  )]
  return(res)
}

#  Add 'plan' to the grouping
calc_district_costs <- function(dt, ints, unit_costs, start_yr, end_yr) {
  dt_sub <- as.data.table(dt)[year >= start_yr & year <= end_yr]
  
  dt_sub[, row_cost := 0]
  for(int in ints) {
    act <- paste0("active_int_", int)
    cov <- paste0("coverage_int_", int)
    dt_sub[, row_cost := row_cost + (nHost * get(act) * get(cov) * unit_costs[[int]])]
  }
  
  #Include 'plan' in both aggregation steps
  costs <- dt_sub[, .(sum_yr = sum(row_cost)), by = .(scenario_name, plan, admin_2, EIR_CI, seed)]
  costs <- costs[, .(avg_cost = mean(sum_yr)), by = .(scenario_name, plan, admin_2, EIR_CI)]
  
  return(costs)
}


# Optimization solver function
run_knapsack <- function(dt_opt, budget) {
  dt_opt <- dt_opt[order(admin_2)]
  unique_subs <- unique(dt_opt$admin_2)
  
  obj <- dt_opt$NMB
  con_budget <- dt_opt$avg_cost
  
  # Matrix: One scenario per district
  sub_indices <- as.numeric(as.factor(dt_opt$admin_2))
  con_mat <- matrix(0, nrow = length(unique_subs), ncol = nrow(dt_opt))
  for(i in 1:nrow(dt_opt)) { con_mat[sub_indices[i], i] <- 1 }
  
  sol <- lp("max", obj, rbind(con_budget, con_mat), c("<=", rep("=", length(unique_subs))), c(budget, rep(1, length(unique_subs))), all.bin = TRUE)
  
  if(sol$status == 0) return(dt_opt[sol$solution == 1, ])
  return(NULL)
}



OptimalAllocation <- function(df,
                              budget_env = 2*10^6,
                              region_name = "setting",
                              policy_name = "scenario_name",
                              cost_name = "tot.mean_total_int_cost",
                              health_name = "ca.tot.mean_tUncomp",
                              optim_dir = "max",
                              keep_col = NULL) {

  if (!optim_dir %in% c("max", "min")) {
    stop("'optim_dir' must be either 'max' or 'min'.", call. = FALSE)
  }

  # Trying to Copy the data so we don't rename columns in the original data_tza
  working_df <- copy(as.data.table(df))

  if (!is.null(keep_col)) {
    keep_col <- c(region_name, policy_name, health_name, cost_name, keep_col)
    working_df <- working_df[, ..keep_col]
  }

  # Standardize column names
  setnames(working_df, old = c(region_name, policy_name), new = c("region", "policy"))

  working_df[, N_policies := .N, by = region]
  working_df[, N_regions := uniqueN(region)]

  df_idx <- unique(working_df[, .(region, N_policies)])[
    , `:=`(
      start = cumsum(N_policies) - N_policies + 1,
      stop  = cumsum(N_policies)
    )
  ]

  objective_coeffs <- working_df[[health_name]]
  constr_budget_env <- matrix(working_df[[cost_name]], nrow = 1, byrow = TRUE)

  constr_per_region <- matrix(0, nrow = unique(working_df$N_regions), ncol = nrow(working_df))
  for(i in 1:nrow(constr_per_region)) {
    constr_per_region[i, c(df_idx$start[i]:df_idx$stop[i])] <- 1
  }

  constr_per_region_policy <- diag(rep(1, nrow(working_df)))
  constraint_matrix <- rbind(constr_budget_env, constr_per_region, constr_per_region_policy)

  rhs <- c(budget_env, rep(1, nrow(constr_per_region)), rep(0, nrow(constr_per_region_policy)))
  constraint_dir <- c("<=", rep("<=", nrow(constr_per_region)), rep(">=", nrow(constr_per_region_policy)))

  solution <- lpSolve::lp(direction = optim_dir,
                          objective.in = objective_coeffs,
                          const.mat = constraint_matrix,
                          const.dir = constraint_dir,
                          const.rhs = rhs)

  working_df[, policy_allocation := solution$solution]

  return(working_df)
}


get_best_allocation <- function(opt_res) {
  
  opt_res[policy_allocation > 0, .SD[which.max(policy_allocation)], by = region]
}

# This is to extract the chosen scenarios from the optimization results
get_optimal_choices <- function(opt_df) {
  # The  function returns 'policy_allocation'. 
  # We filter for the selected policies (allocation > 0)
  setDT(opt_df)
  # In case of fractional ties, we pick the one with the highest allocation
  res <- opt_df[policy_allocation > 0]
  res <- res[.SD[which.max(policy_allocation)], by = region]
  return(res)
}



# server.R

server <- function(input, output, session) {
  
  # 1. Dynamic Inputs for 11 interventions
  output$cost_inputs <- renderUI({
    lapply(int_names, function(i) {
      numericInput(paste0("uc_", i), paste("Unit Cost:", i), value = 2, min = 0)
    })
  })
  
  # 2. REACTIVE ENGINE: Metric Calculation
  metrics_data <- reactive({
    req(input$ref_plan, input$wtp, input$years)
    
    u_costs <- setNames(lapply(int_names, function(i) {
      val <- input[[paste0("uc_", i)]]
      if (is.null(val)) return(5) else return(val)
    }), int_names)
    
    impact <- calc_district_impact(data_tza, input$years[1], input$years[2])
    costs <- calc_district_costs(data_tza, int_names, u_costs, input$years[1], input$years[2])
    
    m <- merge(impact, costs, by = c("scenario_name", "plan", "admin_2", "EIR_CI"))
    
    ref <- m[plan == input$ref_plan, .(admin_2, EIR_CI, r_c = cases_in_period, r_cost = avg_cost)]
    m <- merge(m, ref, by = c("admin_2", "EIR_CI"))
    
    m[, `:=`(
      averted = r_c - cases_in_period,
      d_cost = avg_cost - r_cost
    )]
    
    m[, NMB := (averted * input$wtp) - d_cost]
    m[, is_CE := NMB >= 0]
    
    return(m)
  })
  budget_metrics <- reactive({
    req(input$ref_plan, metrics_data())
    
    # Identify the Target based on Reference selection
    target_p <- ifelse(input$ref_plan == "BAU", "NSP", "BAU")
    
    # Calculate Current Budget based on the Target Plan's current cost
    res_mean <- metrics_data()[EIR_CI == "EIR_mean"]
    curr <- res_mean[plan == target_p, sum(avg_cost, na.rm = TRUE)]
    
    # Calculate Envelope
    env <- curr * (1 + (input$budget_adj/100))
    
    list(curr = curr, env = env, target = target_p)
  })
  
  #  Standardized Join
  get_map_obj <- function(data_subset) {
    shape_file_tza_clean <- shape_file_tza %>% mutate(join_id = toupper(trimws(as.character(JOIN_TARGET))))
    data_clean <- as.data.frame(data_subset) %>% mutate(join_id = toupper(trimws(as.character(!!sym(sub_level_col)))))
    return(left_join(shape_file_tza_clean, data_clean, by = "join_id"))
  }
  
  
  # TAB 1: Most cost effective plan
  output$map_ce <- renderLeaflet({
    d <- metrics_data()[EIR_CI == "EIR_mean" & is_CE == TRUE]
    req(nrow(d) > 0)
    
    # Pick the best scenario for each district
    best_scen <- d[, .SD[which.max(NMB)], by = admin_2]
    
    # Determining the color group
    # We use the 'plan' variable to check the scenario type
    best_scen[, color_group := ifelse(plan == "BAU", "BAU", "NSP")]
    
    map_obj <- get_map_obj(best_scen) %>%
      mutate(
        disp_label = paste0(JOIN_TARGET, " | Recommended: ", scenario_name)
      )
    
    # Define the 2-color palette
    pal <- colorFactor(
      palette = c("#756bb1", "#2ca25f"), # Light Gray and Emerald Green
      levels = c("BAU", "NSP")
    )
    
    leaflet(map_obj) %>% addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(fillColor = ~pal(color_group), weight = 1, color = "white", fillOpacity = 0.8, label = ~disp_label) %>%
      addPolylines(data = tza_outline, color = "black", weight = 2) %>%
      addLegend(pal = pal, values = ~color_group, title = "Strategy Type")
  })
 
  
  
  output$map_opt_assess <- renderLeaflet({
    # Use the reactive budget metrics instead of the deleted input
    req(budget_metrics())
    target_p <- budget_metrics()$target
    curr_budget <- budget_metrics()$curr
    
    res_mean <- metrics_data()[EIR_CI == "EIR_mean"]
    
    # Run Optimization using the automatically identified target
    opt_res <- OptimalAllocation(
      df = res_mean,
      budget_env = curr_budget,
      region_name = "admin_2",
      policy_name = "scenario_name",
      cost_name = "avg_cost",
      health_name = "averted",
      optim_dir = "max"
    )
    
    opt_choices <- get_best_allocation(opt_res)
    validate(need(nrow(opt_choices) > 0, "No optimal allocation found."))
    
    # Compare Current (Target Plan at BAU levels) vs Optimal
    current_scens <- res_mean[plan == target_p, .(admin_2, current_scen = scenario_name)]
    comparison <- merge(current_scens, opt_choices[, .(admin_2 = region, opt_scen = policy)], by = "admin_2")
    
    # Logic for Color: Purple for Keep, Green for Switch
    comparison[, color_group := ifelse(current_scen == opt_scen, "BAU", "Switch to Optimized Plan")]
    
    map_obj <- get_map_obj(comparison) %>%
      mutate(disp_label = paste0(JOIN_TARGET, ": ", color_group))
    
    # Define the Custom Palette
    pal <- colorFactor(
      palette = c("#756bb1", "#2ca25f"), # Purple for Keep, Green for Switch
      levels = c("BAU", "Switch to Optimized Plan")
    )
    
    leaflet(map_obj) %>% addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(fillColor = ~pal(color_group), weight = 1, color = "white", fillOpacity = 0.8, label = ~disp_label) %>%
      addPolylines(data = tza_outline, color = "black", weight = 2) %>%
      addLegend(pal = pal, values = ~color_group, title = "Optimal Action")
  })
  # TAB 3: Optimal allocation (facets Varying map)
 
  output$map_facets <- renderPlot({
    req(budget_metrics())
    target_p <- budget_metrics()$target
    envelope <- budget_metrics()$env
    
    res_mean <- metrics_data()[EIR_CI == "EIR_mean"]
    
    # Run Optimization
    opt_res <- OptimalAllocation(
      df = res_mean,
      budget_env = envelope,
      region_name = "admin_2",
      policy_name = "scenario_name",
      cost_name = "avg_cost",
      health_name = "averted",
      optim_dir = "max"
    )
    
    opt_choices <- get_best_allocation(opt_res)
    req(nrow(opt_choices) > 0)
    
    #  Get the raw intervention data for the chosen scenarios
    scen_info <- data_tza %>% 
      distinct(admin_2, scenario_name, .keep_all = TRUE) %>% 
      select(admin_2, scenario_name, starts_with("active_int_"))
    
    plot_dt <- left_join(as.data.frame(opt_choices), scen_info, 
                         by = c("region" = "admin_2", "policy" = "scenario_name"))
    
    # COMBINE GROUPS (Logical OR: if one is TRUE, the group is TRUE)
    plot_dt_grouped <- plot_dt %>%
      mutate(
        `CM & ICCM` = active_int_CM | active_int_ICCM,
        `PMC & SMC` = active_int_PMC | active_int_SMC,
        `Nets (IG2/PBO/STD)` = active_int_IG2_Nets | active_int_PBO_Nets | active_int_STD_Nets,
        `LSM` = active_int_LSM,
        `Vaccine` = active_int_Vaccine,
        `IPTSc` = active_int_IPTSc,
        `IRS` = active_int_IRS
      ) %>%
      select(admin_2=region, `CM & ICCM`, `PMC & SMC`, `Nets (IG2/PBO/STD)`, LSM, Vaccine, IPTSc, IRS)
    
    # Reshaping to long format and remove any NAs
    plot_dt_long <- plot_dt_grouped %>%
      pivot_longer(cols = -admin_2, names_to = "Intervention_Group", values_to = "ActiveStatus") %>%
      
    # filter(!is.na(Intervention_Group)) # Removes the NA map
      filter(!is.na(Intervention_Group), !is.na(ActiveStatus)) %>%
      
      mutate(Intervention_Group = as.character(Intervention_Group))
    # Joined to shapefile and Plot
    map_obj <- get_map_obj(plot_dt_long)%>%
    filter(!is.na(Intervention_Group))
    ggplot(map_obj) +
      geom_sf(data = tza_outline, fill = "#f2f2f2", color = "gray80", size = 0.1) +
      geom_sf(aes(fill = ActiveStatus), color = NA) +
      facet_wrap(~Intervention_Group, ncol = 3, drop = TRUE) + # Organized grid
      scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "transparent"), 
                        labels = c("TRUE" = "Deployed", "FALSE" = "Not Deployed"), name = "Intervention Status", guide = "legend") +
      theme_void() + 
      theme(strip.text = element_text(size = 12, face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), legend.position = "bottom",
            legend.title = element_text(face = "bold"),
            legend.text = element_text(size = 10)) +
      labs(title = paste0("Grouped Intervention Strategy (Budget: $", format(round(envelope), big.mark=","), ")"))
  })
  
  # Budget Value Box Outputs
  output$box_budget_curr <- renderValueBox({
    valueBox(
      paste0("$", format(round(budget_metrics()$curr), big.mark=",")),
      paste("Current Budget:", budget_metrics()$target),
      icon = icon("wallet"), color = "blue"
    )
  })
  
  output$box_budget_env <- renderValueBox({
    valueBox(
      paste0("$", format(round(budget_metrics()$env), big.mark=",")),
      "Budget Envelope",
      icon = icon("envelope"), color = "purple"
    )
  })
  
}

shinyApp(ui, server)
