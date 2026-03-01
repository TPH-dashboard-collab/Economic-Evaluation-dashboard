library(shiny)
library(shinydashboard)
library(data.table)
library(sf)
library(leaflet)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
library(lpSolve)

#  data loading and pre-processing 

data_tza <- fread("Economic-Evaluation-dashboard/tza_sample_data.csv")
shape_file_tza <- st_read("Economic-Evaluation-dashboard/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp", quiet = TRUE) %>% 
  st_transform(4326)

# Standardize geographic keys
sub_level_col <- "admin_2"
shape_file_tza <- shape_file_tza %>% 
  rename(JOIN_TARGET = admin_2) %>%
  mutate(join_id = toupper(trimws(as.character(JOIN_TARGET))))

tza_outline <- st_union(shape_file_tza)

# Identify all 11 interventions
all_active_cols <- names(data_tza)[grep("^active_int_", names(data_tza))]
int_names <- gsub("active_int_", "", all_active_cols)

# Ensure Eligibility columns exist
# set.seed(123)
# unique_subs <- unique(data_tza[[sub_level_col]])
# for(int in int_names) {
#   elig_col <- paste0("eligible_int_", int)
#   if(!(elig_col %in% names(data_tza))){
#     lookup <- data.table(s = unique_subs, v = sample(c(TRUE, FALSE), length(unique_subs), replace = TRUE))
#     setnames(lookup, c("s", "v"), c(sub_level_col, elig_col))
#     data_tza <- merge(data_tza, lookup, by = sub_level_col, all.x = TRUE)
#   }
# }


# OPTIMIZATION ENGINE

OptimalAllocation <- function(df, budget_env, region_name = "admin_2", policy_name = "scenario_name",
                              cost_name = "avg_cost", health_name = "averted", optim_dir = "max") {
  working_df <- copy(as.data.table(df))
  setnames(working_df, old = c(region_name, policy_name), new = c("region", "policy"))
  
  working_df[, N_policies := .N, by = region]
  working_df[, N_regions := uniqueN(region)]
  
  df_idx <- unique(working_df[, .(region, N_policies)])[, `:=`(start = cumsum(N_policies) - N_policies + 1, stop = cumsum(N_policies))]
  
  objective_coeffs <- working_df[[health_name]]
  constr_budget_env <- matrix(working_df[[cost_name]], nrow = 1, byrow = TRUE)
  
  constr_per_region <- matrix(0, nrow = unique(working_df$N_regions), ncol = nrow(working_df))
  for(i in 1:nrow(constr_per_region)) {
    constr_per_region[i, c(df_idx$start[i]:df_idx$stop[i])] <- 1
  }
  
  solution <- lpSolve::lp(direction = optim_dir, objective.in = objective_coeffs, 
                          const.mat = rbind(constr_budget_env, constr_per_region), 
                          const.dir = c("<=", rep("=", nrow(constr_per_region))), 
                          const.rhs = c(budget_env, rep(1, nrow(constr_per_region))))
  
  working_df[, policy_allocation := solution$solution]
  return(working_df[policy_allocation > 0.99])
}


# SERVER

server <- function(input, output, session) {
  
  # Extracts the single most optimal intervention scenario for each district from the linear programming results.
  get_best_allocation <- function(opt_res) {
    opt_res[policy_allocation > 0, .SD[which.max(policy_allocation)], by = region]
  }
  # Creates a human-readable summary string of all active interventions (e.g., "IRS, SMC") for every simulated scenario in the 2026 data.
  get_scen_summary <- function() {
    temp <- unique(data_tza[year == 2026, .SD, .SDcols = c("admin_2", "scenario_name", all_active_cols)])
    temp[, active_summary := apply(.SD, 1, function(row) {
      active <- int_names[which(row == 1)]; if(length(active)==0) return("None")
      paste(active, collapse = ", ")
    }), .SDcols = all_active_cols]
    return(temp[, .(admin_2, scenario_name, active_summary)])
  }
  
  get_map_obj <- function(data_subset) {
    data_clean <- as.data.frame(data_subset) %>% mutate(join_id = toupper(trimws(as.character(admin_2))))
    return(left_join(shape_file_tza, data_clean, by = "join_id"))
  }
  
  # This reactive calculates the total intervention costs and cumulative cases averted for each scenario over the user-selected time horizon.
  # It performs an incremental analysis by comparing every scenario against the chosen reference plan to determine additional costs and health gains.
  metrics_data <- reactive({
    req(input$wtp, input$years)
    u_costs <- list(CM=input$u_CM, ICCM=input$u_ICCM, SMC=input$u_SMC, PMC=input$u_PMC, IRS=input$u_IRS,
                    LSM=input$u_LSM, Vaccine=input$u_Vaccine, IPTSc=input$u_IPTSc, 
                    STD_Nets=input$u_STD_Nets, PBO_Nets=input$u_PBO_Nets, IG2_Nets=input$u_IG2_Nets)
    
    dt <- copy(data_tza)[year >= input$years[1] & year <= input$years[2]]
    dt[, r_cost := 0]
    for(int in int_names) {
      act <- paste0("active_int_", int); cov <- paste0("coverage_int_", int)
      dt[, r_cost := r_cost + (nHost * get(act) * get(cov) * u_costs[[int]])]
    }
    costs <- dt[, .(avg_cost = mean(sum(r_cost))), by = .(scenario_name, plan, admin_2, EIR_CI)]
    
    dt_end <- data_tza[year == input$years[2], .(c_e = mean(cum_nUncomp)), by = .(scenario_name, plan, admin_2, EIR_CI)]
    dt_st  <- data_tza[year == (input$years[1]-1), .(c_s = mean(cum_nUncomp)), by = .(scenario_name, plan, admin_2, EIR_CI)]
    impact <- merge(dt_end, dt_st, by = c("scenario_name", "plan", "admin_2", "EIR_CI"), all.x=T)
    impact[, averted_period := c_e - fifelse(is.na(c_s), 0, c_s)]
    
    m <- merge(impact, costs, by = c("scenario_name", "plan", "admin_2", "EIR_CI"))
    ref <- m[plan == input$ref_plan, .(admin_2, EIR_CI, r_c = averted_period, r_cost = avg_cost)]
    m <- merge(m, ref, by = c("admin_2", "EIR_CI"))
    
    m[, `:=`(averted = r_c - averted_period, cost_diff = avg_cost - r_cost)]
    m[, `:=`(NMB = (averted * input$wtp) - cost_diff, is_CE = ((averted * input$wtp) - cost_diff) >= 0)]
    m[, ICER := cost_diff / averted]
    return(m)
  })
  
  budget_metrics <- reactive({
    req(input$ref_plan, metrics_data())
    target_p <- ifelse(input$ref_plan == "BAU", "NSP", "BAU")
    # budget_curr is the cost of Plan A (the baseline/reference)
    curr <- metrics_data()[plan == input$ref_plan & EIR_CI == "EIR_mean", sum(avg_cost, na.rm = TRUE)]
    list(curr = curr, env = curr * (1 + (input$budget_adj/100)), target = target_p)
  })
  
  # Generates an interactive Leaflet map that visualizes the optimization results by categorizing districts into either the baseline (BAU) or the upgraded intervention plan (NSP)
  render_optimized_leaflet <- function(maximize_goal) {
    req(budget_metrics())
    res_mean <- metrics_data()[EIR_CI == "EIR_mean"]
    
    opt_res <- OptimalAllocation(df = res_mean, budget_env = budget_metrics()$curr, 
                                 region_name = "admin_2", policy_name = "scenario_name", 
                                 cost_name = "avg_cost", health_name = maximize_goal, optim_dir = "max")
    
    opt_choices <- get_best_allocation(opt_res)
    lookup <- get_scen_summary()
    opt_choices <- merge(opt_choices[, .(admin_2 = region, scenario_name = policy)], 
                         lookup, by = c("admin_2", "scenario_name"), all.x = TRUE)
    
    all_districts <- data.table(admin_2 = unique(data_tza$admin_2))
    comparison <- merge(all_districts, opt_choices, by = "admin_2", all.x = TRUE)
    
    bau_info <- lookup[scenario_name == tolower(input$ref_plan)]
    comparison <- merge(comparison, bau_info[, .(admin_2, bau_summary = active_summary)], by = "admin_2", all.x = TRUE)
    
    comparison[, final_plan := ifelse(is.na(scenario_name) | scenario_name == tolower(input$ref_plan), "BAU", "NSP")]
    comparison[, final_tools := ifelse(is.na(active_summary), bau_summary, active_summary)]
    
    map_obj <- get_map_obj(comparison) %>%
      mutate(disp_label = lapply(paste0("<b>", JOIN_TARGET, "</b><br>Plan: ", final_plan, "<br>Tools: ", final_tools), htmltools::HTML))
    
    pal <- colorFactor(palette = c("#756bb1", "#2ca25f"), levels = c("BAU", "NSP"))
    
    leaflet(map_obj) %>% addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(fillColor = ~pal(final_plan), weight = 1, color = "white", fillOpacity = 0.8, label = ~disp_label,
                  highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = c("BAU", "NSP"), title = "Optimal Action")
  }
  
  output$map_ce  <- renderLeaflet({ render_optimized_leaflet("NMB") })
  output$map_opt_assess <- renderLeaflet({ render_optimized_leaflet("averted") })
  
  # output$map_facets <- renderPlot({
  #   req(budget_metrics())
  #   opt_res <- OptimalAllocation(df = metrics_data()[EIR_CI == "EIR_mean"], budget_env = budget_metrics()$env, 
  #                                region_name = "admin_2", policy_name = "scenario_name", cost_name = "avg_cost")
  #   
  #   opt_choices <- get_best_allocation(opt_res)
  #   scen_info <- data_tza[year == 2026, .SD, .SDcols = c("admin_2", "scenario_name", all_active_cols)]
  #   plot_dt <- left_join(as.data.frame(opt_choices), scen_info, by = c("region" = "admin_2", "policy" = "scenario_name")) %>%
  #     mutate(`CM & ICCM` = factor(case_when(active_int_CM & active_int_ICCM ~ "Both", active_int_CM ~ "CM Only", active_int_ICCM ~ "iCCM Only", TRUE ~ "None")),
  #            `PMC & SMC` = factor(case_when(active_int_PMC & active_int_SMC ~ "Both", active_int_PMC ~ "PMC Only", active_int_SMC ~ "SMC Only", TRUE ~ "None")),
  #            `Nets` = factor(case_when((active_int_IG2_Nets + active_int_PBO_Nets + active_int_STD_Nets) > 1 ~ "Multiple", active_int_IG2_Nets ~ "IG2", active_int_PBO_Nets ~ "PBO", active_int_STD_Nets ~ "STD", TRUE ~ "None")),
  #            LSM = factor(ifelse(active_int_LSM, "Deployed", "None")),
  #            Vaccine = factor(ifelse(active_int_Vaccine, "Deployed", "None")),
  #            IPTSc = factor(ifelse(active_int_IPTSc, "Deployed", "None")),
  #            IRS = factor(ifelse(active_int_IRS, "Deployed", "None"))) %>%
  #     select(admin_2 = region, `CM & ICCM`, `PMC & SMC`, Nets, LSM, Vaccine, IPTSc, IRS)
  #   
  #   plot_dt_long <- pivot_longer(plot_dt, cols = -admin_2, names_to = "Group", values_to = "Status") %>% filter(Status != "None")
  #   map_obj <- get_map_obj(plot_dt_long) %>% filter(!is.na(Group))
  #   
  #   ggplot(map_obj) + geom_sf(data = tza_outline, fill = "#f2f2f2", color = "gray90", size = 0.1) +
  #     geom_sf(aes(fill = Status), color = NA) + facet_wrap(~Group, ncol = 3, drop = TRUE) +
  #     scale_fill_brewer(palette = "Set1", name = "Status") + theme_void() + 
  #     theme(strip.text = element_text(size = 12, face = "bold"), legend.position = "bottom")
  # })
  # 
  # 
  
  output$map_facets <- renderPlot({
    req(budget_metrics())
    
    # Optimization 
    opt_res <- OptimalAllocation(df = metrics_data()[EIR_CI == "EIR_mean"], 
                                 budget_env = budget_metrics()$env, 
                                 region_name = "admin_2", policy_name = "scenario_name", cost_name = "avg_cost")
    
    opt_choices <- get_best_allocation(opt_res)
    scen_info <- data_tza[year == 2026, .SD, .SDcols = c("admin_2", "scenario_name", all_active_cols)]
    
    # Data Prep for plotting
    plot_dt <- left_join(as.data.frame(opt_choices), scen_info, by = c("region" = "admin_2", "policy" = "scenario_name")) %>%
      mutate(
        `CM & ICCM` = factor(case_when(active_int_CM & active_int_ICCM ~ "Both", active_int_CM ~ "CM Only", active_int_ICCM ~ "iCCM Only", TRUE ~ "None")),
        `PMC & SMC` = factor(case_when(active_int_PMC & active_int_SMC ~ "Both", active_int_PMC ~ "PMC Only", active_int_SMC ~ "SMC Only", TRUE ~ "None")),
        `Nets` = factor(case_when((active_int_IG2_Nets + active_int_PBO_Nets + active_int_STD_Nets) > 1 ~ "Multiple", active_int_IG2_Nets ~ "IG2", active_int_PBO_Nets ~ "PBO", active_int_STD_Nets ~ "STD", TRUE ~ "None")),
        LSM = factor(ifelse(active_int_LSM, "Deployed", "None")),
        Vaccine = factor(ifelse(active_int_Vaccine, "Deployed", "None")),
        IPTSc = factor(ifelse(active_int_IPTSc, "Deployed", "None")),
        IRS = factor(ifelse(active_int_IRS, "Deployed", "None"))
      ) %>%
      select(admin_2 = region, `CM & ICCM`, `PMC & SMC`, Nets, LSM, Vaccine, IPTSc, IRS)
    
    plot_dt_long <- pivot_longer(plot_dt, cols = -admin_2, names_to = "Group", values_to = "Status") %>% 
      filter(Status != "None")
    
    map_obj <- get_map_obj(plot_dt_long) %>% filter(!is.na(Group))
    
    
    # We define specific colors for each status to ensure consistency across facets
    status_colors <- c(
      # Case Management (Reds)
      "Both" = "#7b241c", "CM Only" = "#c0392b", "iCCM Only" = "#e74c3c",
      # Nets (Blues)
      "PBO" = "#21618c", "IG2" = "#2e86c1", "STD" = "#5dade2", "Multiple" = "#154360",
      # SMC/PMC (Purples/Greens)
      "SMC Only" = "#6c3483", "PMC Only" = "#1d8348",
      # Binary Deployed (Teals/Dark Gray)
      "Deployed" = "#138d75"
    )
    
    
    ggplot(map_obj) + 
      # Base layer: Background of Tanzania (Light Gray)
      geom_sf(data = tza_outline, fill = "#f2f2f2", color = "white", size = 0.05) +
      # Active Footprint layer
      geom_sf(aes(fill = Status), color = NA) + 
      facet_wrap(~Group, ncol = 3, drop = TRUE) +
      # Use the manual scale instead of Brewer
      scale_fill_manual(values = status_colors, name = "Intervention Status") + 
      theme_void() + 
      theme(
        strip.text = element_text(size = 13, face = "bold", margin = margin(b = 10)),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        panel.spacing = unit(2, "lines"), # Increase space between maps
        plot.margin = margin(20, 20, 20, 20)
      ) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) # Makes legend cleaner
  })
  
  # Budget planner reactive 
  planner_results <- eventReactive(input$run_planner, {
    req(metrics_data())
    res_mean <- metrics_data()[EIR_CI == "EIR_mean"]
    
    #  Run Optimization
    opt_res <- OptimalAllocation(
      df = res_mean, 
      budget_env = input$user_budget_amount,
      region_name = "admin_2",
      policy_name = "scenario_name", 
      cost_name = "avg_cost", 
      health_name = "averted", 
      optim_dir = "max"
    )
    
    lookup <- get_scen_summary()
    bau_info <- lookup[scenario_name == tolower(input$ref_plan)]
    all_districts <- data.table(admin_2 = unique(data_tza$admin_2))
    
    
    # If the solver returns 0 rows, it means we must stay on BAU for everything
    if(nrow(opt_res) == 0) {
      # Use the BAU baseline for every district
      final_choices <- res_mean[plan == input$ref_plan, .(admin_2, scenario_name, cost_val = avg_cost, health_val = 0)]
    } else {
      # Use the Optimized selection
      final_choices <- opt_res[, .(admin_2 = region, scenario_name = policy, cost_val = avg_cost, health_val = averted)]
    }
    
    # Final Merge for Map Labels
    comparison <- merge(all_districts, final_choices, by = "admin_2", all.x = TRUE)
    comparison <- merge(comparison, lookup, by = c("admin_2", "scenario_name"), all.x = TRUE)
    
    # Adding BAU fallback names for inteventions
    comparison <- merge(comparison, bau_info[, .(admin_2, bau_summary = active_summary)], by = "admin_2", all.x = TRUE)
    comparison[, final_tools := ifelse(is.na(active_summary), bau_summary, active_summary)]
    comparison[, final_plan := ifelse(scenario_name == tolower(input$ref_plan), "BAU", "NSP")]
    
    return(comparison)
  })
  
  output$map_planner <- renderLeaflet({
    req(planner_results())
    data <- planner_results()
    map_obj <- get_map_obj(data) %>%
      mutate(disp_label = lapply(paste0("<b>", JOIN_TARGET, "</b><br>Plan: ", final_plan, "<br>Tools: ", final_tools), htmltools::HTML))
    pal <- colorFactor(palette = c("#756bb1", "#2ca25f"), levels = c("BAU", "NSP"))
    leaflet(map_obj) %>% addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(fillColor = ~pal(final_plan), weight = 1, color = "white", fillOpacity = 0.7, label = ~disp_label,
                  highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = c("BAU", "NSP"), title = "Planner Strategy")
  })
  
  output$planner_health_box <- renderValueBox({ valueBox(format(round(sum(planner_results()$health_val, na.rm=T)), big.mark=","), "Total Cases Averted", color = "green") })
  output$planner_cost_box <- renderValueBox({ valueBox(paste0("$", format(round(sum(planner_results()$cost_val, na.rm=T)), big.mark=",")), "Actual Plan Cost", color = "blue") })
  
  # Display from the dashboard 
  output$box_budget_curr <- renderValueBox({ valueBox(paste0("$", format(round(budget_metrics()$curr), big.mark=",")), paste("Current Budget:", input$ref_plan), icon = icon("wallet"), color = "blue") })
  output$box_budget_env <- renderValueBox({ valueBox(paste0("$", format(round(budget_metrics()$env), big.mark=",")), "Budget Envelope", icon = icon("envelope"), color = "purple") })
  output$table_cea <- renderDT({ datatable(metrics_data()[EIR_CI == "EIR_mean", .(admin_2, scenario_name, plan, NMB, ICER, is_CE)]) %>% formatCurrency('NMB', "$") %>% formatRound('ICER', 2) })
}

shinyApp(ui, server)