# This server.R
# Contains only Shiny reactive logic and output renderers.
# All pure functions (optimisation, metrics, plotting, mapping) stays in utils.R.
#
# Reactive dependency graph (simplified):
#
#   [sidebar inputs] → debounced inputs
#       
#   metrics_data()       — per-scenario health & cost metrics
#       
#   budget_metrics()     — BAU-anchored budget envelope
#       
#   opt_res_NMB()        — LP result: maximise NMB      → Map 1, Tornado 1
#   opt_res_averted()    — LP result: maximise averted  → Map 2, Tornado 2,
#                                                          
#       
#   sensitivity_results() — LP re-solved at each budget step → Sensitivity tab

library(shiny)
library(shinydashboard)
library(data.table)
library(sf)
library(leaflet)
library(ggplot2)
library(plotly)
library(DT)
library(lpSolve)
library(htmltools)

source("Economic-Evaluation-dashboard/utils.R")

#  GLOBAL DATA & PRE-COMPUTATION (runs once at startup) 

# Load datasets
data_tza  <- fread("Economic-Evaluation-dashboard/tza_sample_data.csv")
data_tza1 <- fread("Economic-Evaluation-dashboard/TZ_subset_10regions_1seed.csv")

# Load   district shapefile
shape_file_tza <- st_read(
  "Economic-Evaluation-dashboard/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp",
  quiet = TRUE
) |> st_transform(4326)

# Standardise join key on shapefile: upper-cased, trimmed district name
shape_file_tza <- as.data.table(shape_file_tza)
shape_file_tza[, join_id := toupper(trimws(as.character(admin_2)))]
setnames(shape_file_tza, "admin_2", "JOIN_TARGET")
shape_file_tza <- st_as_sf(shape_file_tza)

# National outline for facet map background
tza_outline <- st_union(shape_file_tza)

# Intervention column names derived from data
all_active_cols <- names(data_tza1)[grep("^active_int_", names(data_tza1))]
int_names       <- gsub("active_int_", "", all_active_cols)

# Legacy intervention columns (used only in facet map for data_tza)
all_active_cols_legacy <- names(data_tza)[grep("^active_int_", names(data_tza))]

# Pre-compute scenario summaries and intervention counts (static; never changes)
scen_lookup     <- build_scen_lookup(data_tza1, all_active_cols, int_names)
scen_int_counts <- build_scen_int_counts(scen_lookup)

# Budget sensitivity steps — asymmetric:
# Negative side: steps of 5 (fewer bars, drop only around -25%)
# Positive side: steps of 2 (finer resolution to capture jump at +6%)
SENS_STEPS <- c(seq(-30, -5, by = 5), seq(0, 10, by = 2))


# SERVER 

server <- function(input, output, session) {
  

  # All sidebar inputs are debounced by 800 ms to prevent the optimizer
  # from re-running on every intermediate slider position during drag.
  
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
  
  
  # THE CORE REACTIVE: PER-SCENARIO METRICS 
  # Recomputes whenever any sidebar input changes (after debounce).
  # All downstream reactives depend on this.
  
  metrics_data <- reactive({
    req(wtp_d(), years_d(), input$ref_plan, age_group_d(),
        u_CM_d(), u_ICCM_d(), u_SMC_d(), u_PMC_d(), u_IRS_d(), u_LSM_d(),
        u_Vaccine_d(), u_IPTSc_d(), u_STD_d(), u_PBO_d(), u_IG2_d())
    
    # Collect unit costs into a named list for compute_metrics()
    u_costs <- list(
      CM       = u_CM_d(),
      ICCM     = u_ICCM_d(),
      SMC      = u_SMC_d(),
      PMC      = u_PMC_d(),
      IRS      = u_IRS_d(),
      LSM      = u_LSM_d(),
      Vaccine  = u_Vaccine_d(),
      IPTSc    = u_IPTSc_d(),
      STD_Nets = u_STD_d(),
      PBO_Nets = u_PBO_d(),
      IG2_Nets = u_IG2_d()
    )
    
    # Age group: "all" expands to every unique value in the dataset
    age_groups <- if (age_group_d() == "all") unique(data_tza1$age_group) else age_group_d()
    
    compute_metrics(
      data_tza1  = data_tza1,
      int_names  = int_names,
      u_costs    = u_costs,
      year_start = years_d()[1],
      year_end   = years_d()[2],
      age_groups = age_groups,
      ref_scen   = tolower(input$ref_plan),
      wtp        = wtp_d()
    )
  })
  
  
  # BUDGET ENVELOPE 
  # Always anchored to BAU cost; budget_adj_d() scales it up or down.
  # Changing the reference plan does NOT change the budget — ensuring a fair
  # comparison between BAU and NSP optimizer results.
  
  budget_metrics <- reactive({
    req(metrics_data())
    compute_budget(metrics_data(), budget_adj_d(), tolower(input$ref_plan))
  })
  
  
  # LP OPTIMISATION RESULTS 
  # Two LP solves per   input change — one per objective (NMB vs cases averted).
  # Both use the same budget envelope so results are directly comparable.
  # opt_res_facet is an alias for opt_res_averted (identical solve).
  
  # Pre-deduplicated EIR_mean slice one row per admin_2 x scenario_name
  # Used by all LP solves to ensure consistent results
  res_mean_dedup <- reactive({
    req(metrics_data())
    metrics_data()[EIR_CI == "EIR_mean",
                   .SD[1],
                   by = .(admin_2, scenario_name)]
  })
  
  opt_res_NMB <- reactive({
    req(budget_metrics(), res_mean_dedup())
    OptimalAllocation(
      df          = res_mean_dedup(),
      budget_env  = budget_metrics()$env,
      region_name = "admin_2",
      policy_name = "scenario_name",
      cost_name   = "avg_cost",
      health_name = "NMB",
      optim_dir   = "max"
    )
  })
  
  opt_res_averted <- reactive({
    req(budget_metrics(), res_mean_dedup())
    OptimalAllocation(
      df          = res_mean_dedup(),
      budget_env  = budget_metrics()$env,
      region_name = "admin_2",
      policy_name = "scenario_name",
      cost_name   = "avg_cost",
      health_name = "averted",
      optim_dir   = "max"
    )
  })
  
  # Facet map reuses the health-optimised result — no extra LP solve
  opt_res_facet <- opt_res_averted
  
  
  # BUDGET SENSITIVITY 
  # Re-solves the LP at each SENS_STEPS increment up to the slider value.
  # Results are cached by Shiny's reactive system.
  
  sensitivity_results <- reactive({
    req(budget_metrics(), res_mean_dedup())
    
    run_sensitivity(
      res_mean    = res_mean_dedup(),
      base_budget = budget_metrics()$curr,
      max_adj     = budget_adj_d(),
      sens_steps  = SENS_STEPS
    )
  })
  
  
  # PLANNER (event-driven — only runs when button is pressed)
  
  planner_results <- eventReactive(input$run_planner, {
    req(metrics_data(), res_mean_dedup())
    
    # Apply averted >= 0 filter prevents LP selecting scenarios worse than reference
    # Reference scenario (averted = 0) is always retained
    res_mean       <- res_mean_dedup()[averted >= 0]
    entered_budget <- input$user_budget_amount
    ref_scen       <- tolower(input$ref_plan)
    ref_info       <- scen_lookup[tolower(scenario_name) == ref_scen]
    all_districts  <- data.table(admin_2 = unique(data_tza1$admin_2))
    
    opt_res <- OptimalAllocation(
      df          = res_mean,
      budget_env  = entered_budget,
      region_name = "admin_2",
      policy_name = "scenario_name",
      cost_name   = "avg_cost",
      health_name = "averted",
      optim_dir   = "max"
    )
    
    if (nrow(opt_res) == 0L) {
      # Budget too low — fall back to reference plan for all districts
      final_choices <- res_mean[
        tolower(scenario_name) == ref_scen,
        .(admin_2, scenario_name, cost_val = avg_cost, health_val = 0)
      ]
      warning_msg <- paste0("Budget of $", format(round(entered_budget), big.mark = ","),
                            " is below minimum required. Showing reference plan.")
    } else {
      final_choices <- opt_res[, .(
        admin_2       = region,
        scenario_name = policy,
        cost_val      = avg_cost,
        health_val    = averted
      )]
      warning_msg <- NULL
    }
    
    comparison <- merge(all_districts, final_choices,  by = "admin_2", all.x = TRUE)
    comparison <- merge(comparison,    scen_lookup,     by = c("admin_2", "scenario_name"), all.x = TRUE)
    comparison <- merge(comparison,
                        ref_info[, .(admin_2, ref_summary = active_summary)],
                        by = "admin_2", all.x = TRUE)
    
    comparison[, final_tools := fifelse(is.na(active_summary), ref_summary, active_summary)]
    
    comparison[, final_plan := mapply(
      classify_district,
      opt_scenario = fifelse(is.na(scenario_name), ref_scen, scenario_name),
      ref_scenario = ref_scen,
      MoreArgs     = list(scen_int_counts = scen_int_counts)
    )]
    
    # Attach metadata for display
    attr(comparison, "entered_budget") <- entered_budget
    attr(comparison, "warning_msg")    <- warning_msg
    comparison
  })
  
  
  # CREATED HELPERS TOO BE USED BY MULTIPLE RENDERERS 
  
  # Returns the single best allocation row per region from an LP result
  get_best_allocation <- function(opt_res) {
    opt_res[policy_allocation > 0, .SD[which.max(policy_allocation)], by = region]
  }
  
  # Builds a Leaflet choropleth map for Map 1 (NMB) or Map 2 (averted)
  render_optimized_leaflet <- function(maximize_goal) {
    req(budget_metrics())
    
    opt_res     <- if (maximize_goal == "NMB") opt_res_NMB() else opt_res_averted()
    opt_choices <- get_best_allocation(opt_res)
    ref_scen    <- tolower(input$ref_plan)
    
    # Merge intervention summaries onto LP selections
    opt_choices <- merge(
      opt_choices[, .(admin_2 = region, scenario_name = policy)],
      scen_lookup,
      by    = c("admin_2", "scenario_name"),
      all.x = TRUE
    )
    
    # Ensure all districts are represented (some may not appear in LP result)
    all_districts <- data.table(admin_2 = unique(data_tza1$admin_2))
    comparison    <- merge(all_districts, opt_choices, by = "admin_2", all.x = TRUE)
    
    # Add reference intervention summary for tooltip
    ref_info   <- scen_lookup[tolower(scenario_name) == ref_scen]
    comparison <- merge(comparison,
                        ref_info[, .(admin_2, ref_summary = active_summary)],
                        by = "admin_2", all.x = TRUE)
    
    # Classify each district (Reference / Added / Reduced / Substituted)
    comparison[, final_plan := mapply(
      classify_district,
      opt_scenario = fifelse(is.na(scenario_name), ref_scen, scenario_name),
      ref_scenario = ref_scen,
      MoreArgs     = list(scen_int_counts = scen_int_counts)
    )]
    
    # Merge metric values for tooltip display
    opt_metrics <- opt_res[, .(
      admin_2       = region,
      scenario_name = policy,
      NMB           = NMB,
      averted       = averted,
      avg_cost      = avg_cost,
      ICER          = ICER,
      is_CE         = is_CE
    )]
    comparison <- merge(comparison, opt_metrics, by = c("admin_2", "scenario_name"), all.x = TRUE)
    comparison[, final_tools := fifelse(is.na(active_summary), ref_summary, active_summary)]
    
    #  Build HTML tooltips  different fields shown depending on optimizer goal
    if (maximize_goal == "NMB") {
      map_obj <- build_map_obj(comparison, shape_file_tza)
      map_obj$disp_label <- lapply(paste0(
        "<b>", map_obj$JOIN_TARGET, "</b><br>",
        "<b>Status:</b> ", map_obj$final_plan, "<br>",
        "<hr style='margin:4px 0;'>",
        "<b>&#128203; Reference (", input$ref_plan, "):</b> ", map_obj$ref_summary, "<br>",
        "<b>&#10003; Optimal Selected:</b> ", map_obj$final_tools, "<br>",
        "<hr style='margin:4px 0;'>",
        "<b>NMB:</b> $", ifelse(is.na(map_obj$NMB), "N/A",
                                format(round(map_obj$NMB), big.mark = ",")), "<br>",
        "<b>Cost-Effective:</b> ", ifelse(is.na(map_obj$is_CE), "N/A",
                                          ifelse(map_obj$is_CE, "Yes \u2713", "No \u2717")), "<br>",
        "<b>ICER:</b> ", ifelse(is.na(map_obj$ICER), "N/A",
                                paste0("$", format(round(map_obj$ICER, 2), big.mark = ",")))
      ), htmltools::HTML)
    } else {
      map_obj <- build_map_obj(comparison, shape_file_tza)
      map_obj$disp_label <- lapply(paste0(
        "<b>", map_obj$JOIN_TARGET, "</b><br>",
        "<b>Status:</b> ", map_obj$final_plan, "<br>",
        "<hr style='margin:4px 0;'>",
        "<b>&#128203; Reference (", input$ref_plan, "):</b> ", map_obj$ref_summary, "<br>",
        "<b>&#10003; Optimal Selected:</b> ", map_obj$final_tools, "<br>",
        "<hr style='margin:4px 0;'>",
        "<b>Cases Averted vs Reference:</b> ",
        ifelse(is.na(map_obj$averted), "N/A",
               format(round(map_obj$averted), big.mark = ",")), "<br>",
        "<b>Plan Cost:</b> $",
        ifelse(is.na(map_obj$avg_cost), "N/A",
               format(round(map_obj$avg_cost), big.mark = ","))
      ), htmltools::HTML)
    }
    
    # Colour palette: fixed 4 levels for consistent legend across all views
    all_labels <- c("Reference", "Added", "Reduced", "Substituted")
    pal <- colorFactor(
      palette = c("#756bb1", "#2ca25f", "#e67e22", "#2980b9"),
      levels  = all_labels
    )
    
    leaflet(map_obj) |>
      addProviderTiles(providers$CartoDB.PositronNoLabels) |>
      addPolygons(
        fillColor        = ~pal(final_plan),
        weight           = 1,
        color            = "white",
        fillOpacity      = 0.8,
        label            = ~disp_label,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE)
      ) |>
      addLegend(
        pal    = pal,
        values = all_labels,
        title  = HTML("Optimal vs Reference<br>
                       <small style='font-weight:normal;font-style:italic;'>
                       Hover district for interventions</small>")
      )
  }
  
  
  #  OUTPUT RENDERERS
  
  # Map 1: NMB optimizer
  output$map_ce <- renderLeaflet({
    render_optimized_leaflet("NMB")
  })
  
  # Map 2: Health (cases averted) optimizer
  output$map_opt_assess <- renderLeaflet({
    render_optimized_leaflet("averted")
  })
  
  # Tornado chart — Map 1 (NMB optimizer selections)
  output$tornado_nmb <- renderPlot({
    req(opt_res_NMB())
    make_tornado(opt_res_NMB(), scen_lookup)
  })
  
  # Tornado chart — Map 2 (health optimizer selections)
  output$tornado_averted <- renderPlot({
    req(opt_res_averted())
    make_tornado(opt_res_averted(), scen_lookup)
  })
  
  # Budget value box — BAU baseline cost
  output$box_budget_ref <- renderValueBox({
    valueBox(
      value    = paste0("$", format(round(budget_metrics()$curr), big.mark = ",")),
      subtitle = "Budget Envelope — anchored to BAU cost (fixed comparator)",
      icon     = icon("wallet"),
      color    = "blue"
    )
  })
  
  # Budget value box; adjusted envelope (drives all maps)
  output$box_budget_env <- renderValueBox({
    adj  <- budget_metrics()$adj_pct
    sign <- ifelse(adj >= 0, "+", "")
    valueBox(
      value    = paste0("$", format(round(budget_metrics()$env), big.mark = ","),
                        " (", sign, adj, "%)"),
      subtitle = "Active Budget Envelope — drives Maps 1 & 2, Tornado & Facet Map",
      icon     = icon("envelope"),
      color    = "purple"
    )
  })
  
  # Cases value box, total cases under reference plan (all 10 districts)
  output$box_cases_ref <- renderValueBox({
    ref_total <- metrics_data()[
      EIR_CI == "EIR_mean" & tolower(scenario_name) == tolower(input$ref_plan),
      sum(averted_period, na.rm = TRUE)
    ]
    valueBox(
      value    = format(round(ref_total), big.mark = ","),
      subtitle = paste0("Total Cases Occurring — Reference Plan (", input$ref_plan, ")"),
      icon     = icon("user"),
      color    = "blue"
    )
  })
  
  # Cases value box; optimal plan cases and cases averted vs reference
  output$box_cases_opt <- renderValueBox({
    opt <- opt_res_averted()
    req(opt)
    
    ref_total     <- metrics_data()[
      EIR_CI == "EIR_mean" & tolower(scenario_name) == tolower(input$ref_plan),
      sum(averted_period, na.rm = TRUE)
    ]
    cases_averted <- sum(opt$averted, na.rm = TRUE)
    cases_opt     <- ref_total - cases_averted
    pct_red       <- round(cases_averted / ref_total * 100, 1)
    
    valueBox(
      value    = paste0(format(round(cases_opt), big.mark = ","),
                        " (", format(round(cases_averted), big.mark = ","),
                        " Cases Averted vs reference)"),
      subtitle = paste0("Total Cases Occurring — Optimal Plan  |  ",
                        pct_red, "% reduction vs ", input$ref_plan),
      icon     = icon("shield-virus"),
      color    = "green"
    )
  })
  
  # Track which bar was clicked — must be declared before the renderPlotly
  clicked_step <- reactiveVal(0L)   # default to 0% (baseline)
  
  # Plotly interactive sensitivity bar chart
  # Only renders when Budget Sensitivity tab is active, ensures plot exists
  # before the click observer tries to read event_data
  output$sensitivity_bar <- renderPlotly({
    req(input$dash_tabs == "Budget Sensitivity")
    res <- sensitivity_results()
    
    if (length(res) == 0L) {
      return(plotly_empty() |>
               layout(title = "Move the Budget Change slider to reveal bars"))
    }
    
    # Extract efficiency frontier metadata from run_sensitivity
    actual_cost_pct <- attr(res, "actual_cost_pct")
    actual_cost     <- attr(res, "actual_cost")
    base_budget     <- attr(res, "base_budget")
    
    # Build ordered data table
    df <- data.table(
      pct    = sapply(res, `[[`, "pct"),
      budget = sapply(res, `[[`, "budget"),
      cases  = sapply(res, `[[`, "cases_averted")
    )
    setorder(df, pct)
    
    df[, x_label    := paste0(ifelse(pct >= 0, "+", ""), pct, "%")]
    df[, budget_lbl := paste0("$", round(budget / 1e6, 1), "M")]
    df[, cases_label := ifelse(
      abs(cases) >= 1e6, paste0(round(cases/1e6, 2), "M"),
      ifelse(abs(cases) >= 1e3, paste0(round(cases/1e3, 1), "K"),
             format(round(cases), big.mark = ",")))]
    
    # Colours: red = cut, gold = baseline, blue = increase
     df[, bar_color := fcase(
      pct <  0L, "#e74c3c",
      pct == 0L, "#f39c12",
      default    = "#2980b9"
    )]
    
    df[, hover_text := paste0(
      "<b>Budget change: ", x_label, "</b><br>",
      "Envelope: ", budget_lbl, "<br>",
      "Cases Averted: ", cases_label, "<br>",
      "<i>Click to show map below</i>"
    )]
    
    df[, x_label := factor(x_label, levels = x_label)]
    
    # Find efficiency frontier position, the bar JUST BEFORE cases averted drops
    # This is more reliable than computing actual_cost_pct and matching to x-axis
    baseline_cases <- df[pct == 0, cases]
    # Find the most negative pct where cases still equals baseline
    flat_bars   <- df[cases >= baseline_cases * 0.99]  # allow 1% tolerance
    drop_bars   <- df[cases <  baseline_cases * 0.99]
    
    if (nrow(drop_bars) > 0 && nrow(flat_bars) > 0) {
      # Frontier is between last drop bar and first flat bar (going left to right)
      last_drop_pct  <- max(drop_bars$pct)
      first_flat_pct <- min(flat_bars$pct)
      # Place line halfway between them
      frontier_pct   <- (last_drop_pct + first_flat_pct) / 2
      frontier_x_pos <- mean(c(
        which(levels(df$x_label) == paste0(ifelse(last_drop_pct  >= 0,"+",""), last_drop_pct,  "%")) - 1,
        which(levels(df$x_label) == paste0(ifelse(first_flat_pct >= 0,"+",""), first_flat_pct, "%")) - 1
      ))
      show_frontier <- TRUE
    } else {
      show_frontier  <- FALSE
      frontier_x_pos <- 0
    }
    
    # Format actual cost for annotation
    actual_cost_fmt <- paste0("$", round(actual_cost / 1e6, 1), "M")
    savings_fmt     <- paste0("$", round((base_budget - actual_cost) / 1e6, 1), "M")
    
    # Build the plot explicitly so event_register receives the object directly
    p <- plot_ly(
      data         = df,
      x            = ~x_label,
      y            = ~cases,
      type         = "bar",
      marker       = list(color = ~bar_color,
                          line  = list(color = "white", width = 1.5)),
      text         = ~cases_label,
      textposition = "outside",
      hovertext    = ~hover_text,
      hoverinfo    = "text",
      source       = "sens_bar"
    ) |>
      layout(
        title  = list(text = "Cases Averted at Each Budget Level", x = 0),
        xaxis  = list(
          title         = "Budget Step (% vs BAU reference cost)",
          tickangle     = -45,
          categoryorder = "array",
          categoryarray = levels(df$x_label)
        ),
        yaxis  = list(
          title     = "Cases Averted vs Reference",
          rangemode = "tozero"
        ),
        showlegend    = FALSE,
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin        = list(t = 80),
        shapes        = if (show_frontier) list(
          list(
            type  = "line",
            x0    = frontier_x_pos, x1 = frontier_x_pos,
            y0    = 0,              y1 = 1,
            yref  = "paper",
            line  = list(color = "#27ae60", dash = "dash", width = 2.5)
          )
        ) else list(),
        annotations = if (show_frontier) list(
          list(
            x          = frontier_x_pos,
            y          = 1.12,
            yref       = "paper",
            xref       = "x",
            text       = paste0("<b>Efficiency Frontier</b><br>",
                                "Optimal plan costs ", actual_cost_fmt,
                                " (saves ", savings_fmt, " vs BAU)<br>",
                                "Same health outcome for less spend.<br>",
                                "Cases drop below this line."),
            showarrow  = TRUE,
            arrowhead  = 2,
            arrowcolor = "#27ae60",
            ax         = 80,
            ay         = -50,
            font       = list(size = 11, color = "#27ae60"),
            bgcolor    = "rgba(255,255,255,0.9)",
            bordercolor = "#27ae60",
            borderwidth = 1,
            align      = "left"
          )
        ) else list()
      )
    
    # event_register must receive the plot object as first argument
    event_register(p, "plotly_click")
  })
  
  # Update clicked_step when user clicks a bar
  # Only listen for clicks when the sensitivity tab is active
  # This prevents the "not registered" warning on startup
  observe({
    req(input$dash_tabs == "Budget Sensitivity")
    click <- event_data("plotly_click", source = "sens_bar")
    req(click)
    lbl <- as.character(click$x)
    pct <- as.numeric(gsub("[+%]", "", lbl))
    if (!is.na(pct)) clicked_step(pct)
  })
  
  # Sensitivity sub-tab budget label — shows clicked or current step
  output$sensitivity_budget_label <- renderUI({
    pct         <- clicked_step()
    bud         <- budget_metrics()$curr * (1 + pct / 100)
    sign        <- ifelse(pct >= 0, "+", "")
    actual_cost <- attr(sensitivity_results(), "actual_cost")
    actual_pct  <- attr(sensitivity_results(), "actual_cost_pct")
    savings     <- budget_metrics()$curr - actual_cost
    
    div(
      style = "text-align:center; margin: 6px 0 4px 0;",
      strong(paste0("Facet map showing: Budget at ", sign, pct,
                    "% = $", format(round(bud), big.mark = ","))),
      br(),
      p(HTML(paste0(
        "<span style='color:#27ae60;font-weight:bold;'>&#9646; Efficiency insight:</span> ",
        "The optimal plan only costs <b>$", format(round(actual_cost/1e6, 1)), "M</b> ",
        "(", ifelse(actual_pct >= 0, "+", ""), actual_pct, "% vs BAU), ",
        "saving <b>$", format(round(savings/1e6, 1)), "M</b> vs the BAU budget ",
        "while achieving the same cases averted. ",
        "Cases averted only drops below the efficiency frontier (green dashed line)."
      )),
      style = "font-size:12px; color:#444; margin-top:6px;"),
      p("Click any bar above to update the map below.",
        style = "font-size:11px; color:#888; font-style:italic;")
    )
  })
  
  # Facet map, responds to clicked bar (or current slider if no click yet)
  output$sensitivity_facet <- renderPlot({
    pct      <- if (!is.null(clicked_step()) && clicked_step() != 0) clicked_step() else budget_adj_d()
    bud      <- budget_metrics()$curr * (1 + pct / 100)
    
    opt_res <- tryCatch(
      OptimalAllocation(
        df          = res_mean_dedup(),
        budget_env  = bud,
        region_name = "admin_2",
        policy_name = "scenario_name",
        cost_name   = "avg_cost",
        health_name = "averted",
        optim_dir   = "max"
      ),
      error = function(e) NULL
    )
    
    if (is.null(opt_res) || nrow(opt_res) == 0L) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, size = 6, color = "#888",
                   label = "Budget too low — all districts remain on reference plan") +
          theme_void()
      )
    }
    
    opt_choices  <- get_best_allocation(opt_res)
    plot_dt_long <- prepare_facet_data(opt_choices, data_tza1, all_active_cols)
    make_facet_map(plot_dt_long, shape_file_tza, tza_outline)
  })
  
  # Planner map
  output$map_planner <- renderLeaflet({
    req(planner_results())
    data    <- planner_results()
    map_obj <- build_map_obj(data, shape_file_tza)
    
    map_obj$disp_label <- lapply(paste0(
      "<b>", map_obj$JOIN_TARGET, "</b><br>",
      "<b>Status:</b> ", map_obj$final_plan, "<br>",
      "<b>Interventions deployed:</b> ", map_obj$final_tools
    ), htmltools::HTML)
    
    all_labels <- c("Reference", "Added", "Reduced", "Substituted")
    pal <- colorFactor(
      palette = c("#756bb1", "#2ca25f", "#e67e22", "#2980b9"),
      levels  = all_labels
    )
    
    leaflet(map_obj) |>
      addProviderTiles(providers$CartoDB.PositronNoLabels) |>
      addPolygons(
        fillColor        = ~pal(final_plan),
        weight           = 1,
        color            = "white",
        fillOpacity      = 0.7,
        label            = ~disp_label,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE)
      ) |>
      addLegend(
        pal    = pal,
        values = all_labels,
        title  = HTML("Planner Strategy<br>
                       <small style='font-weight:normal;font-style:italic;'>
                       Hover district for interventions</small>")
      )
  })
  
  # Planner value boxes
  # Budget entered by user
  output$planner_budget_box <- renderValueBox({
    entered <- attr(planner_results(), "entered_budget")
    bau     <- budget_metrics()$curr
    diff    <- entered - bau
    sign    <- ifelse(diff >= 0, "+", "-")
    fmt_usd <- function(x) paste0("$", prettyNum(round(x), big.mark = ","))
    valueBox(
      value    = fmt_usd(entered),
      subtitle = paste0("Budget Entered  |  ",
                        sign, prettyNum(round(abs(diff)), big.mark = ","),
                        " vs BAU reference (", fmt_usd(bau), ")"),
      icon     = icon("dollar-sign"),
      color    = "yellow"
    )
  })
  
  # Unspent budget explanation ()
  output$planner_savings_box <- renderValueBox({
    data        <- planner_results()
    actual_cost <- sum(data$cost_val, na.rm = TRUE)
    entered     <- attr(data, "entered_budget")
    unspent     <- entered - actual_cost
    if (unspent > 100) {
      valueBox(
        value    = paste0("$", formatC(round(unspent), format = "f",
                                       digits = 0, big.mark = ",")),
        subtitle = "Unspent Budget  |  No better scenario in remaining funds. Reallocate to other priorities.",
        icon     = icon("piggy-bank"),
        color    = "purple"
      )
    } else {
      valueBox(
        value    = "$0",
        subtitle = "Unspent Budget  |  Budget fully utilised by optimizer",
        icon     = icon("check"),
        color    = "green"
      )
    }
  })
  
  output$planner_health_box <- renderValueBox({
    data          <- planner_results()
    cases_averted <- sum(data$health_val, na.rm = TRUE)
    warn          <- attr(data, "warning_msg")
    valueBox(
      value    = formatC(round(cases_averted), format = "f",
                         digits = 0, big.mark = ","),
      subtitle = if (!is.null(warn)) warn else
        paste0("Cases Averted vs ", input$ref_plan, " reference"),
      icon     = icon("user-minus"),
      color    = if (cases_averted > 0) "green" else "red"
    )
  })
  
  output$planner_cost_box <- renderValueBox({
    data           <- planner_results()
    actual_cost    <- sum(data$cost_val, na.rm = TRUE)
    entered_budget <- attr(data, "entered_budget")
    unspent        <- entered_budget - actual_cost
    unspent_pct    <- round(unspent / entered_budget * 100, 1)
    subtitle <- if (unspent > 100) {
      paste0("Actual plan cost  |  $",
             formatC(round(unspent), format = "f", digits = 0, big.mark = ","),
             " (", unspent_pct, "%) unspent — no better scenario fits remaining budget")
    } else {
      "Actual plan cost  |  Budget fully utilised"
    }
    valueBox(
      value    = paste0("$", formatC(round(actual_cost), format = "f",
                                     digits = 0, big.mark = ",")),
      subtitle = subtitle,
      icon     = icon("wallet"),
      color    = "blue"
    )
  })
  
  # CEA data table (hidden in UI but retained for future use)
  output$table_cea <- renderDT({
    datatable(
      metrics_data()[EIR_CI == "EIR_mean",
                     .(admin_2, scenario_name, NMB, ICER, is_CE)]
    ) |>
      formatCurrency("NMB", "$") |>
      formatRound("ICER", 2)
  })
  
}

shinyApp(ui, server)



