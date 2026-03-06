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

#data pre-processing 
#data_tza  <- fread("Economic-Evaluation-dashboard/tza_sample_data.csv")
data_tza1 <- fread("Economic-Evaluation-dashboard/TZ_subset_10regions_1seed.csv")

shape_file_tza <- st_read("Economic-Evaluation-dashboard/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp", quiet = TRUE) %>%
  st_transform(4326)

#  Geographic  standardization  
shape_file_tza <- shape_file_tza %>%
  rename(JOIN_TARGET = admin_2) %>%
  mutate(join_id = toupper(trimws(as.character(JOIN_TARGET))))

tza_outline <- st_union(shape_file_tza)



# Using data_tza1 as the primary dataset for optimization (all ~386 combinations)
all_active_cols <- names(data_tza1)[grep("^active_int_", names(data_tza1))]
int_names       <- gsub("active_int_", "", all_active_cols)

# Keep data_tza active cols for facet map (legacy)
all_active_cols_legacy <- names(data_tza1)[grep("^active_int_", names(data_tza1))]
int_names_legacy       <- gsub("active_int_", "", all_active_cols_legacy)

# OPTIMIZATION ENGINE 

OptimalAllocation <- function(df, budget_env, region_name = "admin_2", policy_name = "scenario_name",
                              cost_name = "avg_cost", health_name = "averted", optim_dir = "max") {
  working_df <- copy(as.data.table(df))
  setnames(working_df, old = c(region_name, policy_name), new = c("region", "policy"))
  
  working_df[, N_policies := .N,        by = region]
  working_df[, N_regions  := uniqueN(region)]
  
  df_idx <- unique(working_df[, .(region, N_policies)])[
    , `:=`(start = cumsum(N_policies) - N_policies + 1,
           stop  = cumsum(N_policies))]
  
  objective_coeffs  <- working_df[[health_name]]
  constr_budget_env <- matrix(working_df[[cost_name]], nrow = 1, byrow = TRUE)
  
  constr_per_region <- matrix(0, nrow = unique(working_df$N_regions), ncol = nrow(working_df))
  for (i in seq_len(nrow(constr_per_region))) {
    constr_per_region[i, df_idx$start[i]:df_idx$stop[i]] <- 1
  }
  
  solution <- lpSolve::lp(
    direction    = optim_dir,
    objective.in = objective_coeffs,
    const.mat    = rbind(constr_budget_env, constr_per_region),
    const.dir    = c("<=", rep("=", nrow(constr_per_region))),
    const.rhs    = c(budget_env, rep(1, nrow(constr_per_region)))
  )
  
  working_df[, policy_allocation := solution$solution]
  return(working_df[policy_allocation > 0.99])
}

# SERVER 

server <- function(input, output, session) {
  
  
  get_best_allocation <- function(opt_res) {
    opt_res[policy_allocation > 0, .SD[which.max(policy_allocation)], by = region]
  }
  
  # Building human-readable intervention summary from active_int_* columns
  # Works on either dataset by detecting available active cols
  get_scen_summary <- function(src = data_tza1) {
    a_cols <- names(src)[grep("^active_int_", names(src))]
    i_nms  <- gsub("active_int_", "", a_cols)
    temp   <- unique(src[year == min(src$year, na.rm = TRUE),
                         .SD, .SDcols = c("admin_2", "scenario_name", a_cols)])
    temp[, active_summary := apply(.SD, 1, function(row) {
      active <- i_nms[which(row == 1)]
      if (length(active) == 0) return("No additional interventions")
      paste(active, collapse = ", ")
    }), .SDcols = a_cols]
    return(temp[, .(admin_2, scenario_name, active_summary)])
  }
  
  get_map_obj <- function(data_subset) {
    data_clean <- as.data.frame(data_subset) %>%
      mutate(join_id = toupper(trimws(as.character(admin_2))))
    left_join(shape_file_tza, data_clean, by = "join_id")
  }
  
  # Reference / Changed (replaces BAU / NSP) 
  
  # Converts internal plan codes to display labels
  # display_label <- function(plan_vec) {
  #   dplyr::case_when(
  #     tolower(plan_vec) == "bau"        ~ "Reference",
  #     tolower(plan_vec) == "nsp"        ~ "Changed",
  #     tolower(plan_vec) == "reference"  ~ "Reference",
  #     tolower(plan_vec) == "changed"    ~ "Changed",
  #     TRUE                              ~ plan_vec
  #   )
  # }
  # 
  # METRICS (uses data_tza1 for full combination search) 
  
  metrics_data <- reactive({
    req(input$wtp, input$years)
    
    u_costs <- list(
      CM = input$u_CM, ICCM = input$u_ICCM, SMC = input$u_SMC,
      PMC = input$u_PMC, IRS = input$u_IRS, LSM = input$u_LSM,
      Vaccine = input$u_Vaccine, IPTSc = input$u_IPTSc,
      STD_Nets = input$u_STD_Nets, PBO_Nets = input$u_PBO_Nets,
      IG2_Nets = input$u_IG2_Nets
    )
    
    # Work on data_tza1 (full combination space)
    dt <- copy(data_tza1)[year >= input$years[1] & year <= input$years[2]]
    
    # Filter to a single age group to avoid double-counting
    dt <- dt[age_group == "0-100"]
    
    dt[, r_cost := 0]
    for (int in int_names) {
      act <- paste0("active_int_",   int)
      cov <- paste0("coverage_int_", int)
      if (act %in% names(dt) && cov %in% names(dt)) {
        dt[, r_cost := r_cost + (nHost * get(act) * get(cov) * u_costs[[int]])]
      }
    }
    
    costs <- dt[, .(avg_cost = mean(sum(r_cost))), by = .(scenario_name, plan, admin_2, EIR_CI)]
    
    dt_end <- data_tza1[year == input$years[2]  & age_group == "0-100",
                        .(c_e = mean(cum_nUncomp)), by = .(scenario_name, plan, admin_2, EIR_CI)]
    dt_st  <- data_tza1[year == (input$years[1] - 1) & age_group == "0-100",
                        .(c_s = mean(cum_nUncomp)), by = .(scenario_name, plan, admin_2, EIR_CI)]
    
    impact <- merge(dt_end, dt_st, by = c("scenario_name", "plan", "admin_2", "EIR_CI"), all.x = TRUE)
    impact[, averted_period := c_e - fifelse(is.na(c_s), 0, c_s)]
    
    m <- merge(impact, costs, by = c("scenario_name", "plan", "admin_2", "EIR_CI"))
    
    # Reference: always BAU scenario — used only as baseline for incremental analysis
    # Optimization will search across ALL scenarios (BAU + NSP + all Customized combos)
    ref_scen <- tolower(input$ref_plan)  # "bau" or "nsp"
    ref <- m[tolower(scenario_name) == ref_scen,
             .(admin_2, EIR_CI, r_c = averted_period, r_cost = avg_cost)]
    
    # Keep ALL scenarios for optimization, not just the reference plan
    m <- merge(m, ref, by = c("admin_2", "EIR_CI"), all.x = TRUE)
    m[, `:=`(averted   = r_c - averted_period,
             cost_diff = avg_cost - r_cost)]
    m[, `:=`(NMB   = (averted * input$wtp) - cost_diff,
             is_CE = ((averted * input$wtp) - cost_diff) >= 0)]
    m[, ICER := cost_diff / averted]
    return(m)
  })
  
  # BUDGET METRICS 
  
  budget_metrics <- reactive({
    req(input$ref_plan, metrics_data())
    ref_scen <- tolower(input$ref_plan)
    curr <- metrics_data()[tolower(scenario_name) == ref_scen & EIR_CI == "EIR_mean",
                           sum(avg_cost, na.rm = TRUE)]
    env  <- curr * (1 + (input$budget_adj / 100))
    list(curr = curr, env = env, adj_pct = input$budget_adj)
  })
  
  # CLASSIFY DISTRICT: 4 labels based on intervention count comparison
  
  classify_district <- function(opt_scenario, ref_scenario, lookup) {
    # Get active intervention counts for reference and optimal scenarios
    a_cols <- names(data_tza1)[grep("^active_int_", names(data_tza1))]
    
    get_interventions <- function(scen) {
      row <- lookup[scenario_name == scen]
      if (nrow(row) == 0) return(character(0))
      # Parse intervention names from active_summary
      trimws(unlist(strsplit(row$active_summary[1], ",")))
    }
    
    ref_ints <- get_interventions(ref_scenario)
    opt_ints <- get_interventions(opt_scenario)
    
    if (opt_scenario == ref_scenario)               return("Reference")
    if (length(opt_ints) > length(ref_ints))        return("Added")
    if (length(opt_ints) < length(ref_ints))        return("Reduced")
    return("Substituted")  # same count but different mix
  }
  
  # OPTIMIZED LEAFLET MAP RENDERER 
  
  render_optimized_leaflet <- function(maximize_goal) {
    req(budget_metrics())
    res_mean <- metrics_data()[EIR_CI == "EIR_mean"]
    
    # Pass ALL 384 scenarios to optimizer (BAU + NSP + 382 Customized)
    # Reference is used only for incremental metric computation and post labelling
    opt_res <- OptimalAllocation(
      df          = res_mean,
      budget_env  = budget_metrics()$curr,
      region_name = "admin_2", policy_name = "scenario_name",
      cost_name   = "avg_cost", health_name = maximize_goal, optim_dir = "max"
    )
    
    opt_choices <- get_best_allocation(opt_res)
    lookup      <- get_scen_summary(data_tza1)
    
    opt_choices <- merge(
      opt_choices[, .(admin_2 = region, scenario_name = policy)],
      lookup, by = c("admin_2", "scenario_name"), all.x = TRUE
    )
    
    all_districts <- data.table(admin_2 = unique(data_tza1$admin_2))
    comparison    <- merge(all_districts, opt_choices, by = "admin_2", all.x = TRUE)
    
    ref_info <- lookup[tolower(scenario_name) == tolower(input$ref_plan)]
    comparison <- merge(comparison,
                        ref_info[, .(admin_2, ref_summary = active_summary)],
                        by = "admin_2", all.x = TRUE)
    
    # Apply 4-label classification per district
    comparison[, final_plan := mapply(
      classify_district,
      opt_scenario = fifelse(is.na(scenario_name), tolower(input$ref_plan), scenario_name),
      ref_scenario = tolower(input$ref_plan),
      MoreArgs = list(lookup = lookup)
    )]
    
    # Merge NMB and averted values from opt_res into comparison
    opt_metrics <- opt_res[, .(admin_2 = region, scenario_name = policy,
                               NMB      = NMB,
                               averted  = averted,
                               avg_cost = avg_cost,
                               ICER     = ICER,
                               is_CE    = is_CE)]
    
    comparison <- merge(comparison, opt_metrics,
                        by = c("admin_2", "scenario_name"), all.x = TRUE)
    
    # Show actual interventions of optimal scenario; fallback to reference summary if missing
    comparison[, final_tools := ifelse(is.na(active_summary), ref_summary, active_summary)]
    
    # Build tooltip based on which metric is being maximised
    if (maximize_goal == "NMB") {
      map_obj <- get_map_obj(comparison) %>%
        mutate(disp_label = lapply(paste0(
          "<b>", JOIN_TARGET, "</b><br>",
          "<b>Status:</b> ", final_plan, "<br>",
          "<b>Interventions:</b> ", final_tools, "<br>",
          "<b>NMB:</b> $", ifelse(is.na(NMB), "N/A",
                                  format(round(NMB), big.mark = ",")), "<br>",
          "<b>Cost-Effective:</b> ", ifelse(is.na(is_CE), "N/A",
                                            ifelse(is_CE, "Yes ✓", "No ✗")), "<br>",
          "<b>ICER:</b> ", ifelse(is.na(ICER), "N/A",
                                  paste0("$", format(round(ICER, 2), big.mark = ",")))
        ), htmltools::HTML))
    } else {
      map_obj <- get_map_obj(comparison) %>%
        mutate(disp_label = lapply(paste0(
          "<b>", JOIN_TARGET, "</b><br>",
          "<b>Status:</b> ", final_plan, "<br>",
          "<b>Interventions:</b> ", final_tools, "<br>",
          "<b>Cases Averted vs Reference:</b> ",
          ifelse(is.na(averted), "N/A",
                 format(round(averted), big.mark = ",")), "<br>",
          "<b>Plan Cost:</b> $",
          ifelse(is.na(avg_cost), "N/A",
                 format(round(avg_cost), big.mark = ","))
        ), htmltools::HTML))
    }
    
    all_labels <- c("Reference", "Added", "Reduced", "Substituted")
    pal <- colorFactor(
      palette = c("#756bb1", "#2ca25f", "#e67e22", "#2980b9"),
      levels  = all_labels
    )
    
    leaflet(map_obj) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(
        fillColor        = ~pal(final_plan),
        weight           = 1, color = "white", fillOpacity = 0.8,
        label            = ~disp_label,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(
        pal    = pal,
        values = all_labels,
        title  = HTML("Optimal vs Reference<br>
                       <small style='font-weight:normal;font-style:italic;'>
                       Hover district for interventions</small>")
      )
  }
  
  output$map_ce         <- renderLeaflet({ render_optimized_leaflet("NMB") })
  output$map_opt_assess <- renderLeaflet({ render_optimized_leaflet("averted") })
  
  # FACET MAP 
  
  output$map_facets <- renderPlot({
    req(budget_metrics())
    
    
    candidates_facet <- metrics_data()[EIR_CI == "EIR_mean"]
    opt_res <- OptimalAllocation(
      df         = candidates_facet,
      budget_env = budget_metrics()$env,
      region_name = "admin_2", policy_name = "scenario_name",
      cost_name  = "avg_cost"
    )
    
    opt_choices <- get_best_allocation(opt_res)
    scen_info   <- data_tza1[year == min(data_tza1$year) & age_group == "0-100",
                             .SD, .SDcols = c("admin_2", "scenario_name", all_active_cols)]
    
    plot_dt <- left_join(
      as.data.frame(opt_choices), scen_info,
      by = c("region" = "admin_2", "policy" = "scenario_name")
    ) %>%
      mutate(
        `CM & ICCM` = factor(case_when(
          active_int_CM == 1 & active_int_ICCM == 1 ~ "CM + iCCM",
          active_int_CM == 1   ~ "CM Only",
          active_int_ICCM == 1 ~ "iCCM Only",
          TRUE ~ "None")),
        `PMC & SMC` = factor(case_when(
          active_int_PMC == 1 & active_int_SMC == 1 ~ "PMC + SMC",
          active_int_PMC == 1 ~ "PMC Only",
          active_int_SMC == 1 ~ "SMC Only",
          TRUE ~ "None")),
        Nets = factor(case_when(
          (active_int_IG2_Nets + active_int_PBO_Nets + active_int_STD_Nets) > 1 ~ "Multiple Nets",
          active_int_IG2_Nets == 1 ~ "IG2 Nets",
          active_int_PBO_Nets == 1 ~ "PBO Nets",
          active_int_STD_Nets == 1 ~ "STD Nets",
          TRUE ~ "None")),
        LSM     = factor(ifelse(active_int_LSM == 1,     "LSM",     "None")),
        Vaccine = factor(ifelse(active_int_Vaccine == 1, "Vaccine", "None")),
        IPTSc   = factor(ifelse(active_int_IPTSc == 1,   "IPTSc",   "None")),
        IRS     = factor(ifelse(active_int_IRS == 1,     "IRS",     "None"))
      ) %>%
      select(admin_2 = region, `CM & ICCM`, `PMC & SMC`, Nets, LSM, Vaccine, IPTSc, IRS)
    
    plot_dt_long <- pivot_longer(plot_dt, cols = -admin_2,
                                 names_to = "Group", values_to = "Status") %>%
      filter(Status != "None")
    
    map_obj <- get_map_obj(plot_dt_long) %>% filter(!is.na(Group))
    
    status_colors <- c(
      # CM & ICCM (blues/teals)
      "CM Only"      = "#2196F3",
      "iCCM Only"    = "#00796B",
      "CM + iCCM"    = "#1A237E",
      # PMC & SMC (purples/greens)
      "PMC Only"     = "#7B1FA2",
      "SMC Only"     = "#388E3C",
      "PMC + SMC"    = "#4A148C",
      # Nets (blue shades)
      "STD Nets"     = "#90CAF9",
      "PBO Nets"     = "#1565C0",
      "IG2 Nets"     = "#0D47A1",
      "Multiple Nets"= "#01579B",
      # Single interventions — each unique
      "LSM"          = "#F57F17",
      "Vaccine"      = "#C62828",
      "IPTSc"        = "#558B2F",
      "IRS"          = "#6D4C41"
    )
    
    ggplot(map_obj) +
      geom_sf(data = tza_outline, fill = "#f2f2f2", color = "white", size = 0.05) +
      geom_sf(aes(fill = Status), color = NA) +
      facet_wrap(~Group, ncol = 3, drop = TRUE) +
      scale_fill_manual(values = status_colors, name = "Intervention Status",
                        drop = FALSE) +
      theme_void() +
      theme(
        strip.text      = element_text(size = 13, face = "bold", margin = margin(b = 10)),
        legend.position = "bottom",
        legend.title    = element_text(face = "bold"),
        panel.spacing   = unit(2, "lines"),
        plot.margin     = margin(20, 20, 20, 20)
      ) +
      guides(fill = guide_legend(nrow = 3, byrow = TRUE, override.aes = list(size = 5)))
  })
  
  # VALUE BOXES: BUDGET 
  
  output$box_budget_ref <- renderValueBox({
    valueBox(
      value    = paste0("$", format(round(budget_metrics()$curr), big.mark = ",")),
      subtitle = paste("Reference Plan Cost (", input$ref_plan, ")"),
      icon     = icon("wallet"),
      color    = "blue"
    )
  })
  
  output$box_budget_env <- renderValueBox({
    valueBox(
      value    = paste0("$", format(round(budget_metrics()$env), big.mark = ",")),
      subtitle = "Budget Envelope (after adjustment)",
      icon     = icon("envelope"),
      color    = "purple"
    )
  })
  
  output$box_budget_delta <- renderValueBox({
    adj   <- budget_metrics()$adj_pct
    delta <- budget_metrics()$env - budget_metrics()$curr
    sign  <- ifelse(delta >= 0, "+", "")
    col   <- ifelse(adj > 0, "green", ifelse(adj < 0, "red", "black"))
    valueBox(
      value    = paste0(sign, "$", format(round(abs(delta)), big.mark = ","),
                        " (", sign, adj, "%)"),
      subtitle = "Change from Reference Budget",
      icon     = icon(ifelse(adj >= 0, "arrow-up", "arrow-down")),
      color    = ifelse(adj > 0, "green", ifelse(adj < 0, "red", "black"))
    )
  })
  

  
  # Run optimizer and return winning scenarios with averted_period
  get_optimal_cases <- function() {
    res_mean <- metrics_data()[EIR_CI == "EIR_mean"]
    opt_res  <- tryCatch(
      OptimalAllocation(df = res_mean, budget_env = budget_metrics()$curr,
                        region_name = "admin_2", policy_name = "scenario_name",
                        cost_name = "avg_cost", health_name = "averted", optim_dir = "max"),
      error = function(e) NULL
    )
    if (is.null(opt_res) || nrow(opt_res) == 0) return(NULL)
    
    # Get averted_period for each winning scenario (total cases under optimal, not incremental)
    opt_scenarios <- opt_res[, .(admin_2 = region, scenario_name = policy)]
    merged <- merge(opt_scenarios, res_mean[, .(admin_2, scenario_name, averted_period)],
                    by = c("admin_2", "scenario_name"), all.x = TRUE)
    return(merged)
  }
  
  output$box_cases_ref <- renderValueBox({
    ref_scen  <- tolower(input$ref_plan)
    cases_ref <- metrics_data()[tolower(scenario_name) == ref_scen & EIR_CI == "EIR_mean",
                                sum(averted_period, na.rm = TRUE)]
    valueBox(
      value    = format(round(cases_ref), big.mark = ","),
      subtitle = paste0("Total Cases Occurring — Reference Plan (", input$ref_plan, ")"),
      icon     = icon("user"),
      color    = "blue"
    )
  })
  
  output$box_cases_opt <- renderValueBox({
    opt_data  <- get_optimal_cases()
    cases_opt <- if (is.null(opt_data)) 0 else sum(opt_data$averted_period, na.rm = TRUE)
    
    ref_scen  <- tolower(input$ref_plan)
    cases_ref <- metrics_data()[tolower(scenario_name) == ref_scen & EIR_CI == "EIR_mean",
                                sum(averted_period, na.rm = TRUE)]
    
    # Positive = cases averted by switching from reference to optimal
    cases_averted <- cases_ref - cases_opt
    
    valueBox(
      value    = paste0(format(round(cases_opt), big.mark = ","),
                        " (", format(round(cases_averted), big.mark = ","), " cases averted vs reference)"),
      subtitle = "Total Cases Occurring — Optimal Plan (cases averted vs reference in brackets)",
      icon     = icon("shield-virus"),
      color    = "green"
    )
  })
  
  # BUDGET PLANNER 
  
  planner_results <- eventReactive(input$run_planner, {
    req(metrics_data())
    res_mean <- metrics_data()[EIR_CI == "EIR_mean"]
    
    # Pass ALL 384 scenarios to optimizer
    opt_res <- OptimalAllocation(
      df          = res_mean,
      budget_env  = input$user_budget_amount,
      region_name = "admin_2", policy_name = "scenario_name",
      cost_name   = "avg_cost", health_name = "averted", optim_dir = "max"
    )
    
    lookup   <- get_scen_summary(data_tza1)
    ref_info <- lookup[tolower(scenario_name) == tolower(input$ref_plan)]
    all_districts <- data.table(admin_2 = unique(data_tza1$admin_2))
    
    if (nrow(opt_res) == 0) {
      ref_scen <- tolower(input$ref_plan)
      final_choices <- res_mean[tolower(scenario_name) == ref_scen,
                                .(admin_2, scenario_name, cost_val = avg_cost, health_val = 0)]
    } else {
      final_choices <- opt_res[, .(admin_2 = region, scenario_name = policy,
                                   cost_val = avg_cost, health_val = averted)]
    }
    
    comparison <- merge(all_districts, final_choices, by = "admin_2", all.x = TRUE)
    comparison <- merge(comparison, lookup, by = c("admin_2", "scenario_name"), all.x = TRUE)
    comparison <- merge(comparison,
                        ref_info[, .(admin_2, ref_summary = active_summary)],
                        by = "admin_2", all.x = TRUE)
    
    comparison[, final_tools := ifelse(is.na(active_summary), ref_summary, active_summary)]
    
    # Applying 4-label classification
    comparison[, final_plan := mapply(
      classify_district,
      opt_scenario = fifelse(is.na(scenario_name), tolower(input$ref_plan), scenario_name),
      ref_scenario = tolower(input$ref_plan),
      MoreArgs = list(lookup = lookup)
    )]
    return(comparison)
  })
  
  output$map_planner <- renderLeaflet({
    req(planner_results())
    data    <- planner_results()
    map_obj <- get_map_obj(data) %>%
      mutate(disp_label = lapply(
        paste0(
          "<b>", JOIN_TARGET, "</b><br>",
          "<b>Status:</b> ", final_plan, "<br>",
          "<b>Interventions deployed:</b> ", final_tools
        ),
        htmltools::HTML
      ))
    
    all_labels <- c("Reference", "Added", "Reduced", "Substituted")
    pal <- colorFactor(
      palette = c("#756bb1", "#2ca25f", "#e67e22", "#2980b9"),
      levels  = all_labels
    )
    
    leaflet(map_obj) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(
        fillColor        = ~pal(final_plan),
        weight           = 1, color = "white", fillOpacity = 0.7,
        label            = ~disp_label,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(
        pal    = pal,
        values = all_labels,
        title  = HTML("Planner Strategy<br>
                       <small style='font-weight:normal;font-style:italic;'>
                       Hover district for interventions</small>")
      )
  })
  
  output$planner_health_box <- renderValueBox({
    valueBox(
      format(round(sum(planner_results()$health_val, na.rm = TRUE)), big.mark = ","),
      "Total Cases Averted", icon = icon("user-minus"), color = "green"
    )
  })
  
  output$planner_cost_box <- renderValueBox({
    valueBox(
      paste0("$", format(round(sum(planner_results()$cost_val, na.rm = TRUE)), big.mark = ",")),
      "Actual Plan Cost", icon = icon("wallet"), color = "blue"
    )
  })
  
  #  CEA TABLE (hidden in UI for now but retained) 
  output$table_cea <- renderDT({
    datatable(
      metrics_data()[EIR_CI == "EIR_mean",
                     .(admin_2, scenario_name, plan, NMB, ICER, is_CE)]
    ) %>%
      formatCurrency("NMB", "$") %>%
      formatRound("ICER", 2)
  })
}

shinyApp(ui, server)


