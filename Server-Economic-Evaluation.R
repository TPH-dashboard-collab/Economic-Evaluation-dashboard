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

# =============================================================================
# 1. DATA LOADING & PRE-PROCESSING
# =============================================================================
data_tza <- fread("Economic-Evaluation-dashboard/tza_sample_data.csv")
shape_file_tza <- st_read("Economic-Evaluation-dashboard/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp", quiet = TRUE) %>% 
  st_transform(4326)

# Standardize geographic keys immediately
sub_level_col <- "admin_2"
shape_file_tza <- shape_file_tza %>% 
  rename(JOIN_TARGET = admin_2) %>%
  mutate(join_id = toupper(trimws(as.character(JOIN_TARGET))))

tza_outline <- st_union(shape_file_tza)

# Identify all 11 interventions
all_active_cols <- names(data_tza)[grep("^active_int_", names(data_tza))]
int_names <- gsub("active_int_", "", all_active_cols)

# Create a fixed set of eligibility columns (one per district)
set.seed(123)
unique_subs <- unique(data_tza[[sub_level_col]])
for(int in int_names) {
  elig_col <- paste0("eligible_int_", int)
  if(!(elig_col %in% names(data_tza))){
    lookup <- data.table(s = unique_subs, v = sample(c(TRUE, FALSE), length(unique_subs), replace = TRUE))
    setnames(lookup, c("s", "v"), c(sub_level_col, elig_col))
    data_tza <- merge(data_tza, lookup, by = sub_level_col, all.x = TRUE)
  }
}


#  OPTIMIZATION ENGINE 

OptimalAllocation <- function(df, budget_env, region_name = "admin_2", policy_name = "scenario_name",
                              cost_name = "avg_cost", health_name = "averted", optim_dir = "max") {
  working_df <- copy(as.data.table(df))
  setnames(working_df, old = c(region_name, policy_name), new = c("region", "policy"))
  
  working_df[, N_policies := .N, by = region]
  working_df[, N_regions := uniqueN(region)]
  
  df_idx <- unique(working_df[, .(region, N_policies)])[, `:=`(start = cumsum(N_policies) - N_policies + 1, stop = cumsum(N_policies))]
  
  objective_coeffs <- working_df[[health_name]]
  constr_budget_env <- matrix(working_df[[cost_name]], nrow = 1, byrow = TRUE)
  
  # Ensure exactly one scenario per region
  
  constr_per_region <- matrix(0, nrow = unique(working_df$N_regions), ncol = nrow(working_df))
  for(i in 1:nrow(constr_per_region)) {
    constr_per_region[i, c(df_idx$start[i]:df_idx$stop[i])] <- 1
  }
  
  # Solver
  solution <- lpSolve::lp(direction = optim_dir, objective.in = objective_coeffs, 
                          const.mat = rbind(constr_budget_env, constr_per_region), 
                          const.dir = c("<=", rep("=", nrow(constr_per_region))), 
                          const.rhs = c(budget_env, rep(1, nrow(constr_per_region))))
  
  working_df[, policy_allocation := solution$solution]
  return(working_df[policy_allocation > 0.99]) # Snap to binary choices
}




#  SERVER

server <- function(input, output, session) {
  
  # --- A. HELPERS ---
  get_scen_summary <- function() {
    temp <- unique(data_tza[year == 2026, .SD, .SDcols = c("admin_2", "scenario_name", all_active_cols)])
    temp[, active_summary := apply(.SD, 1, function(row) {
      active <- int_names[which(row == 1)]; if(length(active)==0) return("Clinical Case Management")
      paste(active, collapse = ", ")
    }), .SDcols = all_active_cols]
    return(temp[, .(admin_2, scenario_name, active_summary)])
  }
  
  get_map_obj <- function(data_subset) {
    data_clean <- as.data.frame(data_subset) %>% mutate(join_id = toupper(trimws(as.character(admin_2))))
    return(left_join(shape_file_tza, data_clean, by = "join_id"))
  }
  
  #  Making the data reactive 
  metrics_data <- reactive({
    req(input$wtp, input$years)
    u_costs <- list(CM=input$u_CM, ICCM=input$u_ICCM, SMC=input$u_SMC, PMC=input$u_PMC, IRS=input$u_IRS,
                    LSM=input$u_LSM, Vaccine=input$u_Vaccine, IPTSc=input$u_IPTSc, 
                    STD_Nets=input$u_STD_Nets, PBO_Nets=input$u_PBO_Nets, IG2_Nets=input$u_IG2_Nets)
    
    # Cost math: Pop * Active * Coverage * Price
    
    dt <- copy(data_tza)[year >= input$years[1] & year <= input$years[2]]
    dt[, r_cost := 0]
    for(int in int_names) {
      act <- paste0("active_int_", int); cov <- paste0("coverage_int_", int)
      dt[, r_cost := r_cost + (nHost * get(act) * get(cov) * u_costs[[int]])]
    }
    costs <- dt[, .(avg_cost = mean(sum(r_cost))), by = .(scenario_name, plan, admin_2, EIR_CI)]
    
    # Impact math
    dt_end <- data_tza[year == input$years[2], .(c_e = mean(cum_nUncomp)), by = .(scenario_name, plan, admin_2, EIR_CI)]
    dt_st  <- data_tza[year == (input$years[1]-1), .(c_s = mean(cum_nUncomp)), by = .(scenario_name, plan, admin_2, EIR_CI)]
    impact <- merge(dt_end, dt_st, by = c("scenario_name", "plan", "admin_2", "EIR_CI"), all.x=T)
    impact[, cases_in_period := c_e - fifelse(is.na(c_s), 0, c_s)]
    
    m <- merge(impact, costs, by = c("scenario_name", "plan", "admin_2", "EIR_CI"))
    ref <- m[plan == input$ref_plan, .(admin_2, EIR_CI, r_c = cases_in_period, r_cost = avg_cost)]
    m <- merge(m, ref, by = c("admin_2", "EIR_CI"))
    m[, `:=`(averted = r_c - cases_in_period, cost_diff = avg_cost - r_cost)]
    m[, `:=`(NMB = (averted * input$wtp) - cost_diff, is_CE = ((averted * input$wtp) - cost_diff) >= 0)]

    # m <- merge(impact, costs, by = c("scenario_name", "plan", "admin_2", "EIR_CI"))
    # ref <- m[plan == input$ref_plan, .(admin_2, EIR_CI, r_c = cases_in_period, r_cost = avg_cost)]
    # m <- merge(m, ref, by = c("admin_2", "EIR_CI"))
    # 
    # # PERFORM ALL CALCULATIONS IN ONE STEP
    # m[, `:=`(
    #   averted   = r_c - cases_in_period,
    #   cost_diff = avg_cost - r_cost
    # )]
    # 
    # m[, `:=`(
    #   NMB   = (averted * input$wtp) - cost_diff,
    #   is_CE = ((averted * input$wtp) - cost_diff) >= 0,
    #   ICER  = cost_diff / averted
    # )]
    
    return(m)
  })
  
  budget_metrics <- reactive({
    req(input$ref_plan, metrics_data())
    target_p <- ifelse(input$ref_plan == "BAU", "NSP", "BAU")
    curr <- metrics_data()[plan == input$ref_plan & EIR_CI == "EIR_mean", sum(avg_cost, na.rm = TRUE)]
    list(curr = curr, env = curr * (1 + (input$budget_adj/100)), target = target_p)
  })
  
  # map logic: the function switches for both the cases averted and NMB to the optimizer 
  render_optimized_map <- function(goal) {
    req(budget_metrics())
    res_mean <- metrics_data()[EIR_CI == "EIR_mean"]
    
    #  Running  LP Solver
    opt_res <- OptimalAllocation(df = res_mean, budget_env = budget_metrics()$curr, 
                                 region_name = "admin_2", policy_name = "scenario_name", 
                                 cost_name = "avg_cost", health_name = goal, optim_dir = "max")
    
    #  Getting the full list of scenario definitions (BAU and NSP)
    lookup <- get_scen_summary()
    
    #  Getting the BAU definitions specifically to use as a fallback
    bau_defs <- lookup[scenario_name == tolower(input$ref_plan)]
    
    #  To identify the optimizers
    opt_choices <- merge(opt_res[, .(admin_2 = region, scenario_name = policy)], 
                         lookup, by = c("admin_2", "scenario_name"), all.x = TRUE)
    
    # Create the complete map table
    all_districts <- data.table(admin_2 = unique(data_tza$admin_2))
    comparison <- merge(all_districts, opt_choices, by = "admin_2", all.x = TRUE)
    
    # If the solver skipped a district, merge in the ACTUAL BAU scenarios
    comparison <- merge(comparison, bau_defs[, .(admin_2, bau_summary = active_summary)], by = "admin_2", all.x = TRUE)
    
    # Logic: If 'active_summary' is missing, we use the real 'bau_summary' from the data
    comparison[, final_tools := ifelse(is.na(active_summary), bau_summary, active_summary)]
    comparison[, final_plan := ifelse(is.na(scenario_name), "BAU", "NSP")]
    
    # Joining with Shapefile
    map_obj <- get_map_obj(comparison) %>%
      mutate(
        disp_label = lapply(paste0("<b>", JOIN_TARGET, "</b><br>Plan: ", final_plan, "<br>Tools: ", final_tools), htmltools::HTML)
      )
    
    pal <- colorFactor(palette = c("#756bb1", "#2ca25f"), levels = c("BAU", "NSP"))
    
    leaflet(map_obj) %>% addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(fillColor = ~pal(final_plan), weight = 1, color = "white", fillOpacity = 0.8, label = ~disp_label,
                  highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = c("BAU", "NSP"), title = "Optimal Action")
  }
  output$map_ce  <- renderLeaflet({ render_optimized_map("NMB") })
  output$map_opt <- renderLeaflet({ render_optimized_map("averted") })
  
  output$map_facets <- renderPlot({
    req(budget_metrics())
    opt_res <- OptimalAllocation(df = metrics_data()[EIR_CI == "EIR_mean"], budget_env = budget_metrics()$env, 
                                 region_name = "admin_2", policy_name = "scenario_name", cost_name = "avg_cost")
    
    scen_info <- data_tza[year == 2026, .SD, .SDcols = c("admin_2", "scenario_name", all_active_cols)]
    plot_dt <- left_join(as.data.frame(opt_res), scen_info, by = c("region" = "admin_2", "policy" = "scenario_name")) %>%
      mutate(`CM & ICCM` = active_int_CM | active_int_ICCM, `PMC & SMC` = active_int_PMC | active_int_SMC,
             `Nets (IG2/PBO/STD)` = active_int_IG2_Nets | active_int_PBO_Nets | active_int_STD_Nets) %>%
      select(admin_2 = region, `CM & ICCM`, `PMC & SMC`, `Nets (IG2/PBO/STD)`, LSM=active_int_LSM, Vaccine=active_int_Vaccine, IPTSc=active_int_IPTSc, IRS=active_int_IRS)
    
    long_data <- pivot_longer(plot_dt, cols = -admin_2, names_to = "Group", values_to = "Status") %>% filter(!is.na(Group))
    map_obj <- get_map_obj(long_data) %>% filter(!is.na(Group))
    
    ggplot(map_obj) + geom_sf(data = tza_outline, fill = "#f2f2f2", color = "gray80", size = 0.1) +
      geom_sf(aes(fill = Status), color = NA) + facet_wrap(~Group, ncol = 3, drop = TRUE) +
      scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "transparent"), labels = c("TRUE" = "Deployed", "FALSE" = "Not Deployed"), name = "Status") +
      theme_void() + theme(strip.text = element_text(size = 12, face = "bold"), legend.position = "bottom")
  })
  
  # OUTPUTS 
  output$box_budget_curr <- renderValueBox({ valueBox(paste0("$", format(round(budget_metrics()$curr), big.mark=",")), paste("Current Budget:", input$ref_plan), icon = icon("wallet"), color = "blue") })
  output$box_budget_env  <- renderValueBox({ valueBox(paste0("$", format(round(budget_metrics()$env), big.mark=",")), "Budget Envelope", icon = icon("envelope"), color = "purple") })
  #output$table_cea <- renderDT({ datatable(metrics_data()[EIR_CI == "EIR_mean", .(admin_2, scenario_name, plan, NMB, ICER, is_CE)]) %>% formatCurrency('NMB', "$") %>% formatRound('ICER', 2) })
}

shinyApp(ui, server)

































































