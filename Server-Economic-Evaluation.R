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

# DATA LOADING 
# Using the subset data from Tanzania(data_tza1)
data_tza  <- fread("Economic-Evaluation-dashboard/tza_sample_data.csv")
data_tza1 <- fread("Economic-Evaluation-dashboard/TZ_subset_10regions_1seed.csv")

shape_file_tza <- st_read("Economic-Evaluation-dashboard/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp", quiet = TRUE) %>%
  st_transform(4326)



shape_file_tza <- shape_file_tza %>%
  rename(JOIN_TARGET = admin_2) %>%
  mutate(join_id = toupper(trimws(as.character(JOIN_TARGET))))

tza_outline <- st_union(shape_file_tza)



# Use data_tza1 as the primary dataset for optimization (all ~386 combinations)
all_active_cols <- names(data_tza1)[grep("^active_int_", names(data_tza1))]
int_names       <- gsub("active_int_", "", all_active_cols)

# Keep data_tza active cols for facet map 
all_active_cols_legacy <- names(data_tza)[grep("^active_int_", names(data_tza))]
int_names_legacy       <- gsub("active_int_", "", all_active_cols_legacy)

#  OPTIMIZATION ENGINE 

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

#  PRE-COMPUTE SCENARIO SUMMARY ONCE AT STARTUP (never changes) 
# Avoids recomputing inside every reactive on every input change
scen_lookup <- local({
  a_cols <- names(data_tza1)[grep("^active_int_", names(data_tza1))]
  i_nms  <- gsub("active_int_", "", a_cols)
  temp   <- unique(data_tza1[year == min(data_tza1$year),
                             .SD, .SDcols = c("admin_2", "scenario_name", a_cols)])
  temp[, active_summary := apply(.SD, 1, function(row) {
    active <- i_nms[which(row == 1)]
    if (length(active) == 0) return("No additional interventions")
    paste(active, collapse = ", ")
  }), .SDcols = a_cols]
  temp[, .(admin_2, scenario_name, active_summary)]
})

# Pre-compute intervention counts per scenario for classify_district
# Use unique scenario_name only — count commas in active_summary + 1
scen_int_counts <- unique(scen_lookup[, .(scenario_name, active_summary)])[
  , .(n_ints = fifelse(
    active_summary == "No additional interventions", 0L,
    lengths(regmatches(active_summary, gregexpr(",", active_summary))) + 1L
  )), by = scenario_name]

 

# SERVER

server <- function(input, output, session) {
  
  
  
  get_best_allocation <- function(opt_res) {
    opt_res[policy_allocation > 0, .SD[which.max(policy_allocation)], by = region]
  }
  
  # Using pre-computed global lookup 
  get_scen_summary <- function(src = NULL) scen_lookup
  
  get_map_obj <- function(data_subset) {
    data_clean <- as.data.frame(data_subset) %>%
      mutate(join_id = toupper(trimws(as.character(admin_2))))
    left_join(shape_file_tza, data_clean, by = "join_id")
  }
  
  # ── DEBOUNCED INPUTS — wait 800ms after user stops changing before recomputing 
  wtp_d        <- debounce(reactive(input$wtp),        800)
  years_d      <- debounce(reactive(input$years),      800)
  budget_adj_d <- debounce(reactive(input$budget_adj), 800)
  age_group_d  <- debounce(reactive(input$age_group),  800)
  u_CM_d      <- debounce(reactive(input$u_CM),       800)
  u_ICCM_d    <- debounce(reactive(input$u_ICCM),     800)
  u_SMC_d     <- debounce(reactive(input$u_SMC),      800)
  u_PMC_d     <- debounce(reactive(input$u_PMC),      800)
  u_IRS_d     <- debounce(reactive(input$u_IRS),      800)
  u_LSM_d     <- debounce(reactive(input$u_LSM),      800)
  u_Vaccine_d <- debounce(reactive(input$u_Vaccine),  800)
  u_IPTSc_d   <- debounce(reactive(input$u_IPTSc),    800)
  u_STD_d     <- debounce(reactive(input$u_STD_Nets), 800)
  u_PBO_d     <- debounce(reactive(input$u_PBO_Nets), 800)
  u_IG2_d     <- debounce(reactive(input$u_IG2_Nets), 800)
  
  # The Metrics 
  
  metrics_data <- reactive({
    req(wtp_d(), years_d())
    
    u_costs <- list(
      CM = u_CM_d(), ICCM = u_ICCM_d(), SMC = u_SMC_d(),
      PMC = u_PMC_d(), IRS = u_IRS_d(), LSM = u_LSM_d(),
      Vaccine = u_Vaccine_d(), IPTSc = u_IPTSc_d(),
      STD_Nets = u_STD_d(), PBO_Nets = u_PBO_d(),
      IG2_Nets = u_IG2_d()
    )
    
    # Resolve age group filter
    ag <- if (age_group_d() == "all") unique(data_tza1$age_group) else age_group_d()
    
    dt <- copy(data_tza1)[year >= years_d()[1] & year <= years_d()[2] & age_group %in% ag]
    
    dt[, r_cost := 0]
    for (int in int_names) {
      act <- paste0("active_int_",   int)
      cov <- paste0("coverage_int_", int)
      if (act %in% names(dt) && cov %in% names(dt))
        dt[, r_cost := r_cost + (nHost * get(act) * get(cov) * u_costs[[int]])]
    }
    
    # Cost sum across age groups for a single year, then mean across years
    # This gives average annual cost per district x scenario
    costs <- dt[, .(r_cost_yr = sum(r_cost, na.rm = TRUE)),
                by = .(scenario_name, admin_2, EIR_CI, year)][
                  , .(avg_cost = mean(r_cost_yr, na.rm = TRUE)),
                  by = .(scenario_name, admin_2, EIR_CI)]
    
    # cum_nUncomp at year_end = cumulative cases over the full simulation period
    # No need to subtract start year — it is already the period total
    # dt_end <- data_tza1[year == years_d()[2] & age_group %in% ag,
    #                     .(c_e = sum(cum_nUncomp, na.rm = TRUE)),
    #                     by = .(scenario_name, admin_2, EIR_CI)]
    # 
    # impact <- dt_end[, .(scenario_name, admin_2, EIR_CI,
    #                      averted_period = c_e)]
    
    dt_end <- data_tza1[year == years_d()[2]  & age_group %in% ag,
                        .(c_e = mean(cum_nUncomp)), by = .(scenario_name, plan, admin_2, EIR_CI)]
    dt_st  <- data_tza1[year == (years_d()[1] - 1) & age_group %in% ag,
                        .(c_s = mean(cum_nUncomp)), by = .(scenario_name, plan, admin_2, EIR_CI)]
    
    impact <- merge(dt_end, dt_st, by = c("scenario_name", "plan", "admin_2", "EIR_CI"), all.x = TRUE)
    impact[, averted_period := c_e - fifelse(is.na(c_s), 0, c_s)]
    
    
    m      <- merge(impact, costs, by = c("scenario_name", "admin_2", "EIR_CI"))
    ref_scen <- tolower(input$ref_plan)
    ref <- m[tolower(scenario_name) == ref_scen,
             .(admin_2, EIR_CI, r_c = averted_period, r_cost = avg_cost)]
    
    m <- merge(m, ref, by = c("admin_2", "EIR_CI"), all.x = TRUE)
    m[, `:=`(averted   = r_c - averted_period,
             cost_diff = avg_cost - r_cost)]
    m[, `:=`(NMB   = (averted * wtp_d()) - cost_diff,
             is_CE = ((averted * wtp_d()) - cost_diff) >= 0)]
    m[, ICER := cost_diff / averted]
    return(m)
  })
  
  # cases averted per district 
  district_cases <- reactive({
    req(metrics_data())
    m <- metrics_data()[EIR_CI == "EIR_mean"]
    
    # Collapsing to ONE row per admin_2 x scenario_name
    # Use mean averted_period and mean avg_cost in case plan column creates duplicates
    agg <- m[, .(
      averted_period = mean(averted_period, na.rm = TRUE),
      avg_cost       = mean(avg_cost,       na.rm = TRUE)
    ), by = .(admin_2, scenario_name)]
    
    # Reference cases per district — one value per district
    ref_agg <- agg[tolower(scenario_name) == tolower(input$ref_plan),
                   .(admin_2, ref_period = averted_period)]
    
    agg <- merge(agg, ref_agg, by = "admin_2", all.x = TRUE)
    agg[, cases_averted := ref_period - averted_period]
    agg
  })
  
  get_cases_averted <- function(opt_res) {
    req(opt_res, district_cases())
    choices <- data.table(admin_2 = opt_res$region, scenario_name = opt_res$policy)
    merge(choices, district_cases(), by = c("admin_2", "scenario_name"), all.x = TRUE)
  }
  
  budget_metrics <- reactive({
    req(metrics_data())
    # Budget is ALWAYS anchored to BAU cost — regardless of reference plan selected
    # Reference plan only affects the health counterfactual (r_c), not the budget envelope
    # This ensures fair comparison: NSP and BAU optimizer get the same money
    bau_cost <- metrics_data()[tolower(scenario_name) == "bau" & EIR_CI == "EIR_mean",
                               sum(avg_cost, na.rm = TRUE)]
    env <- bau_cost * (1 + (budget_adj_d() / 100))
    list(curr = bau_cost, env = env, adj_pct = budget_adj_d())
  })
  
  # ── SHARED CACHED LP RESULTS — all driven by budget envelope (incl. slider) 
  # budget_metrics()$env = BAU_cost * (1 + budget_adj%)
  # At 0% adjustment: env == curr == BAU cost
  # Slider increase unlocks better scenarios on Maps 1, 2, tornado, and facet map
  
  opt_res_NMB <- reactive({
    req(budget_metrics())
    res_mean <- metrics_data()[EIR_CI == "EIR_mean"]
    OptimalAllocation(df = res_mean, budget_env = budget_metrics()$env,
                      region_name = "admin_2", policy_name = "scenario_name",
                      cost_name = "avg_cost", health_name = "NMB", optim_dir = "max")
  })
  
  opt_res_averted <- reactive({
    req(budget_metrics())
    res_mean <- metrics_data()[EIR_CI == "EIR_mean"]
    OptimalAllocation(df = res_mean, budget_env = budget_metrics()$env,
                      region_name = "admin_2", policy_name = "scenario_name",
                      cost_name = "avg_cost", health_name = "averted", optim_dir = "max")
  })
  
  # opt_res_facet now identical to opt_res_averted — reuse it
  opt_res_facet <- opt_res_averted
  
  # CLASSIFY DISTRICT: 4 labels 
  
  classify_district <- function(opt_scenario, ref_scenario) {
    if (opt_scenario == ref_scenario) return("Reference")
    opt_n <- scen_int_counts[scenario_name == opt_scenario, n_ints][1]
    ref_n <- scen_int_counts[scenario_name == ref_scenario, n_ints][1]
    if (is.na(opt_n) || is.na(ref_n)) return("Substituted")
    if (opt_n > ref_n) return("Added")
    if (opt_n < ref_n) return("Reduced")
    return("Substituted")
  }
  
  # OPTIMIZING LEAFLET MAP RENDERER 
  
  render_optimized_leaflet <- function(maximize_goal) {
    req(budget_metrics())
    
    # Use cached LP result — no re-solve on every render
    opt_res <- if (maximize_goal == "NMB") opt_res_NMB() else opt_res_averted()
    
    opt_choices <- get_best_allocation(opt_res)
    lookup      <- scen_lookup  # use pre-computed global
    
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
    
    # Apply 4-label classification using pre-computed counts (for fast)
    comparison[, final_plan := mapply(
      classify_district,
      opt_scenario = fifelse(is.na(scenario_name), tolower(input$ref_plan), scenario_name),
      ref_scenario = tolower(input$ref_plan)
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
    
    comparison[, final_tools := ifelse(is.na(active_summary), ref_summary, active_summary)]
    
    # Build tooltip based on which metric is being maximised
    if (maximize_goal == "NMB") {
      map_obj <- get_map_obj(comparison) %>%
        mutate(disp_label = lapply(paste0(
          "<b>", JOIN_TARGET, "</b><br>",
          "<b>Status:</b> ", final_plan, "<br>",
          "<hr style='margin:4px 0;'>",
          "<b>&#128203; Reference (", input$ref_plan, "):</b> ", ref_summary, "<br>",
          "<b>&#10003; Optimal Selected:</b> ", final_tools, "<br>",
          "<hr style='margin:4px 0;'>",
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
          "<hr style='margin:4px 0;'>",
          "<b>&#128203; Reference (", input$ref_plan, "):</b> ", ref_summary, "<br>",
          "<b>&#10003; Optimal Selected:</b> ", final_tools, "<br>",
          "<hr style='margin:4px 0;'>",
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
  
  # output$map_facets <- renderPlot({
  #   req(budget_metrics())
  #   
  #   # Use cached facet optimizer result
  #   opt_choices <- get_best_allocation(opt_res_facet())
  #   scen_info   <- data_tza1[year == min(data_tza1$year) & age_group == "0-100",
  #                            .SD, .SDcols = c("admin_2", "scenario_name", all_active_cols)]
  #   
  #   plot_dt <- left_join(
  #     as.data.frame(opt_choices), scen_info,
  #     by = c("region" = "admin_2", "policy" = "scenario_name")
  #   ) %>%
  #     mutate(
  #       `CM & ICCM` = factor(case_when(
  #         active_int_CM == 1 & active_int_ICCM == 1 ~ "CM + iCCM",
  #         active_int_CM == 1   ~ "CM Only",
  #         active_int_ICCM == 1 ~ "iCCM Only",
  #         TRUE ~ "None")),
  #       `PMC & SMC` = factor(case_when(
  #         active_int_PMC == 1 & active_int_SMC == 1 ~ "PMC + SMC",
  #         active_int_PMC == 1 ~ "PMC Only",
  #         active_int_SMC == 1 ~ "SMC Only",
  #         TRUE ~ "None")),
  #       Nets = factor(case_when(
  #         (active_int_IG2_Nets + active_int_PBO_Nets + active_int_STD_Nets) > 1 ~ "Multiple Nets",
  #         active_int_IG2_Nets == 1 ~ "IG2 Nets",
  #         active_int_PBO_Nets == 1 ~ "PBO Nets",
  #         active_int_STD_Nets == 1 ~ "STD Nets",
  #         TRUE ~ "None")),
  #       LSM     = factor(ifelse(active_int_LSM == 1,     "LSM",     "None")),
  #       Vaccine = factor(ifelse(active_int_Vaccine == 1, "Vaccine", "None")),
  #       IPTSc   = factor(ifelse(active_int_IPTSc == 1,   "IPTSc",   "None")),
  #       IRS     = factor(ifelse(active_int_IRS == 1,     "IRS",     "None"))
  #     ) %>%
  #     select(admin_2 = region, `CM & ICCM`, `PMC & SMC`, Nets, LSM, Vaccine, IPTSc, IRS)
  #   
  #   plot_dt_long <- pivot_longer(plot_dt, cols = -admin_2,
  #                                names_to = "Group", values_to = "Status") %>%
  #     filter(Status != "None")
  #   
  #   map_obj <- get_map_obj(plot_dt_long) %>% filter(!is.na(Group))
  #   
  #   status_colors <- c(
  #     # CM & ICCM (blues/teals)
  #     "CM Only"      = "#2196F3",
  #     "iCCM Only"    = "#00796B",
  #     "CM + iCCM"    = "#1A237E",
  #     # PMC & SMC (purples/greens)
  #     "PMC Only"     = "#7B1FA2",
  #     "SMC Only"     = "#388E3C",
  #     "PMC + SMC"    = "#4A148C",
  #     # Nets (blue shades)
  #     "STD Nets"     = "#90CAF9",
  #     "PBO Nets"     = "#1565C0",
  #     "IG2 Nets"     = "#0D47A1",
  #     "Multiple Nets"= "#01579B",
  #     # Single interventions — each unique
  #     "LSM"          = "#F57F17",
  #     "Vaccine"      = "#C62828",
  #     "IPTSc"        = "#558B2F",
  #     "IRS"          = "#6D4C41"
  #   )
  #   
  #   ggplot(map_obj) +
  #     geom_sf(data = tza_outline, fill = "#f2f2f2", color = "white", size = 0.05) +
  #     geom_sf(aes(fill = Status), color = NA) +
  #     facet_wrap(~Group, ncol = 3, drop = TRUE) +
  #     scale_fill_manual(values = status_colors, name = "Intervention Status",
  #                       drop = FALSE) +
  #     theme_void() +
  #     theme(
  #       strip.text      = element_text(size = 13, face = "bold", margin = margin(b = 10)),
  #       legend.position = "bottom",
  #       legend.title    = element_text(face = "bold"),
  #       panel.spacing   = unit(2, "lines"),
  #       plot.margin     = margin(20, 20, 20, 20)
  #     ) +
  #     guides(fill = guide_legend(nrow = 3, byrow = TRUE, override.aes = list(size = 5)))
  # })
  # 
  
  # ── TORNADO CHARTS — uses district_cases() single source of truth ─────────
  
  make_tornado <- function(opt_res) {
    req(opt_res)
    
    # All columns already in LP result
    plot_dt <- data.table(
      admin_2       = opt_res$region,
      scenario_name = opt_res$policy,
      cases_averted = opt_res$averted,        # r_c - averted_period per district
      cases_opt     = opt_res$averted_period, # absolute cases occurring under optimal
      ref_cases     = opt_res$r_c,            # reference cases per district
      avg_cost      = opt_res$avg_cost
    )
    
    # Attach intervention label from scen_lookup
    plot_dt <- merge(plot_dt,
                     unique(scen_lookup[, .(scenario_name, active_summary)]),
                     by = "scenario_name", all.x = TRUE)
    plot_dt[, combo := ifelse(
      is.na(active_summary) | active_summary == "No additional interventions",
      "Reference only", active_summary)]
    
    # Group by intervention combination
    tornado_dt <- plot_dt[, .(
      cases_averted = sum(cases_averted, na.rm = TRUE),
      cost_M        = sum(avg_cost,      na.rm = TRUE) / 1e6
    ), by = combo]
    
    tornado_dt <- tornado_dt[order(cases_averted)]
    tornado_dt[, combo := factor(combo, levels = unique(combo))]
    
    max_cases <- max(abs(tornado_dt$cases_averted), na.rm = TRUE)
    max_cost  <- max(abs(tornado_dt$cost_M),        na.rm = TRUE)
    k <- ifelse(max_cost > 0, max_cases / max_cost, 1)
    tornado_dt[, cost_scaled := -cost_M * k]
    
    fmt_cases <- function(x) {
      ifelse(abs(x) >= 1e6, paste0(round(x/1e6, 2), "M"),
             ifelse(abs(x) >= 1e3, paste0(round(x/1e3, 1), "K"),
                    format(round(x), big.mark = ",")))
    }
    
    ggplot(tornado_dt, aes(y = combo)) +
      geom_col(aes(x = cost_scaled),   fill = "#E8A020", width = 0.55) +
      geom_col(aes(x = cases_averted), fill = "#4B0082", width = 0.55) +
      geom_text(aes(x = cost_scaled,
                    label = paste0("$", round(cost_M, 1), "M")),
                hjust = 1.08, size = 3.3, color = "gray20") +
      geom_text(aes(x = cases_averted,
                    label = fmt_cases(cases_averted),
                    hjust = ifelse(cases_averted >= 0, -0.08, 1.08)),
                size = 3.3, color = "gray20") +
      geom_vline(xintercept = 0, color = "gray40", linewidth = 0.6) +
      scale_x_continuous(
        labels = function(x) {
          ifelse(x >= 0,
                 ifelse(x >= 1e6, paste0(round(x/1e6,1),"M"), paste0(round(x/1e3),"K")),
                 paste0("$", round(abs(x)/k, 1), "M"))
        },
        expand = expansion(mult = c(0.28, 0.2))
      ) +
      labs(x = NULL, y = NULL,
           caption = "\u25A0 Purple = Cases Averted (right)     \u25A0 Gold = Cost in M USD (left)") +
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
  
  output$tornado_nmb <- renderPlot({
    req(opt_res_NMB(), district_cases())
    make_tornado(opt_res_NMB())
  })
  
  output$tornado_averted <- renderPlot({
    req(opt_res_averted(), district_cases())
    make_tornado(opt_res_averted())
  })
  
  output$box_budget_ref <- renderValueBox({
    valueBox(
      value    = paste0("$", format(round(budget_metrics()$curr), big.mark = ",")),
      subtitle = "Budget Envelope — anchored to BAU cost (fixed comparator)",
      icon     = icon("wallet"), color = "blue"
    )
  })
  
  output$box_budget_env <- renderValueBox({
    adj  <- budget_metrics()$adj_pct
    sign <- ifelse(adj >= 0, "+", "")
    valueBox(
      value    = paste0("$", format(round(budget_metrics()$env), big.mark = ","),
                        " (", sign, adj, "%)"),
      subtitle = "Active Budget Envelope — drives Maps 1 & 2, Tornado & Facet Map",
      icon     = icon("envelope"), color = "purple"
    )
  })
  
  
  
  # VALUE BOXES: CASES AVERTED 
  
  output$box_cases_ref <- renderValueBox({
    ref_total <- metrics_data()[EIR_CI == "EIR_mean" &
                                  tolower(scenario_name) == tolower(input$ref_plan),
                                sum(averted_period, na.rm = TRUE)]
    valueBox(
      value    = format(round(ref_total), big.mark = ","),
      subtitle = paste0("Total Cases Occurring — Reference Plan (", input$ref_plan, ")"),
      icon     = icon("user"), color = "blue"
    )
  })
  
  output$box_cases_opt <- renderValueBox({
    opt <- opt_res_averted()
    req(opt)
    ref_total     <- metrics_data()[EIR_CI == "EIR_mean" &
                                      tolower(scenario_name) == tolower(input$ref_plan),
                                    sum(averted_period, na.rm = TRUE)]
    cases_averted <- sum(opt$averted, na.rm = TRUE)
    cases_opt     <- ref_total - cases_averted
    pct_red       <- round(cases_averted / ref_total * 100, 1)
    valueBox(
      value    = paste0(format(round(cases_opt), big.mark = ","),
                        " (", format(round(cases_averted), big.mark = ","),
                        " Cases Averted vs reference)"),
      subtitle = paste0("Total Cases Occurring — Optimal Plan  |  ",
                        pct_red, "% reduction vs ", input$ref_plan),
      icon     = icon("shield-virus"), color = "green"
    )
  })
  
  #  BUDGET SENSITIVITY 
  # Steps: 0% to 30% in 5% increments — up to 7 bars, emerge as slider increases
  
  SENS_STEPS <- c(0, 5, 10, 15, 20, 25, 30)
  
  sensitivity_results <- reactive({
    req(budget_metrics(), metrics_data())
    adj <- budget_adj_d()
    
    if (adj < 0) return(list())
    
    active_steps <- SENS_STEPS[SENS_STEPS <= adj]
    if (length(active_steps) == 0) active_steps <- 0
    
    res_mean    <- metrics_data()[EIR_CI == "EIR_mean"]
    ref_scen    <- tolower(input$ref_plan)
    base_budget <- budget_metrics()$curr
    
    results <- lapply(active_steps, function(pct) {
      bud <- base_budget * (1 + pct / 100)
      opt <- tryCatch(
        OptimalAllocation(df = res_mean, budget_env = bud,
                          region_name = "admin_2", policy_name = "scenario_name",
                          cost_name   = "avg_cost", health_name = "averted",
                          optim_dir   = "max"),
        error = function(e) NULL
      )
      if (is.null(opt) || nrow(opt) == 0) {
        cases_avert <- 0
        opt_choices <- NULL
      } else {
        # Use averted directly from LP result — same source as value box
        cases_avert <- sum(opt$averted, na.rm = TRUE)
        opt_choices <- opt
      }
      list(pct = pct, budget = bud, cases_averted = cases_avert, opt_res = opt_choices)
    })
    
    # Enforce monotonicity — cases averted can never decrease with more budget
    averted_vals <- sapply(results, function(r) r$cases_averted)
    for (i in seq_along(averted_vals)) {
      if (i > 1 && averted_vals[i] < averted_vals[i - 1]) {
        averted_vals[i]            <- averted_vals[i - 1]
        results[[i]]$cases_averted <- averted_vals[i - 1]
      }
    }
    results
  })
  
  output$sensitivity_bar <- renderPlot({
    res <- sensitivity_results()
    
    if (length(res) == 0) {
      return(ggplot() +
               geom_blank() +
               annotate("text", x = 0.5, y = 0.5, size = 5, color = "gray50",
                        label = "Move the Budget Change slider above 0% to reveal bars") +
               theme_void())
    }
    
    pcts  <- sapply(res, function(r) r$pct)
    cases <- sapply(res, function(r) r$cases_averted)
    buds  <- sapply(res, function(r) r$budget)
    
    fmt_cases <- function(x) {
      if (x >= 1e6)      paste0(round(x / 1e6, 2), "M")
      else if (x >= 1e3) paste0(round(x / 1e3, 1), "K")
      else               format(round(x), big.mark = ",")
    }
    
    df <- data.frame(
      idx         = factor(seq_along(pcts)),
      pct         = pcts,
      budget      = buds,
      cases       = cases,
      cases_label = sapply(cases, fmt_cases),
      x_label     = paste0("+", pcts, "%  |  $", round(buds / 1e6, 1), "M"),
      stringsAsFactors = FALSE
    )
    
    ggplot(df, aes(x = idx, y = cases)) +
      geom_col(fill = "#2980b9", color = "white", width = 0.45) +
      geom_text(aes(label = cases_label),
                vjust = -0.5, fontface = "bold", size = 4.5, color = "#1a252f") +
      scale_x_discrete(labels = df$x_label) +
      scale_y_continuous(
        labels = function(y) ifelse(
          y >= 1e6, paste0(round(y / 1e6, 1), "M"),
          paste0(round(y / 1e3, 0), "K")
        ),
        expand = expansion(mult = c(0.02, 0.22))
      ) +
      labs(
        x        = "Budget Step  (% increase vs reference  |  total envelope)",
        y        = "Cases Averted vs Reference",
        title    = "Cases Averted at Each Budget Level",
        subtitle = paste0(length(res), " of 7 steps shown — move slider right to reveal more")
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title         = element_text(face = "bold", size = 14),
        plot.subtitle      = element_text(color = "gray50", size = 11),
        axis.title         = element_text(face = "bold", size = 12),
        axis.text.x        = element_text(size = 10, color = "gray30", margin = margin(t = 6)),
        axis.ticks.x       = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.margin        = margin(12, 20, 16, 20)
      )
  })
  
  # Budget label above facet map — shows current slider budget
  output$sensitivity_budget_label <- renderUI({
    adj  <- budget_adj_d()
    bud  <- budget_metrics()$env
    sign <- ifelse(adj >= 0, "+", "")
    div(
      style = "text-align:center; margin: 6px 0 12px 0;",
      strong(paste0("Facet map showing: Budget at ", sign, adj,
                    "% = $", format(round(bud), big.mark = ",")))
    )
  })
  
  # Facet map — driven by existing opt_res_facet (current budget envelope)
  output$sensitivity_facet <- renderPlot({
    opt_res <- opt_res_facet()
    req(opt_res)
    
    if (is.null(opt_res) || nrow(opt_res) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "Budget too low — all districts remain on reference plan",
                        size = 6, color = "#888") +
               theme_void())
    }
    
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
      "CM Only" = "#2196F3", "iCCM Only" = "#00796B", "CM + iCCM" = "#1A237E",
      "PMC Only" = "#7B1FA2", "SMC Only" = "#388E3C", "PMC + SMC" = "#4A148C",
      "STD Nets" = "#90CAF9", "PBO Nets" = "#1565C0", "IG2 Nets" = "#0D47A1",
      "Multiple Nets" = "#01579B",
      "LSM" = "#F57F17", "Vaccine" = "#C62828", "IPTSc" = "#558B2F", "IRS" = "#6D4C41"
    )
    
    ggplot(map_obj) +
      geom_sf(data = tza_outline, fill = "#f2f2f2", color = "white", size = 0.05) +
      geom_sf(aes(fill = Status), color = NA) +
      facet_wrap(~Group, ncol = 3, drop = TRUE) +
      scale_fill_manual(values = status_colors, name = "Intervention Status", drop = FALSE) +
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
    
    lookup   <- scen_lookup
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
    
    # Apply 4-label classification
    comparison[, final_plan := mapply(
      classify_district,
      opt_scenario = fifelse(is.na(scenario_name), tolower(input$ref_plan), scenario_name),
      ref_scenario = tolower(input$ref_plan)
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


