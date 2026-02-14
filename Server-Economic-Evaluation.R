
library(shiny)
library(shinydashboard)
library(data.table)
library(sf)
library(leaflet)
library(ggplot2)
library(DT)

# Load data
data_tza <- fread("Economic-Evaluation-dashboard/tza_sample_data.csv")
shape_file_tza <- st_read("Economic-Evaluation-dashboard/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp", quiet = TRUE)
shape_file_tza <- st_transform(shape_file_tza, 4326)


# Server
server <- function(input, output, session){
  
  
  int_names <- gsub("active_int_", "", 
                    names(data_tza)[grepl("^active_int_", names(data_tza))])
  
  for (int in int_names) {
    col_name <- paste0("eligible_int_", int)
    data_tza[, (col_name) := sample(c(TRUE, FALSE), 1), by = admin_2]
  }
  
  get_int_names <- function(winning_plan) {
    lookup <- unique(data_tza[age_group == "0-100", .SD, 
                              .SDcols = c("admin_2", "scenario_name", 
                                          paste0("active_int_", int_names))])
    
    res <- merge(winning_plan, lookup, by = c("admin_2", "scenario_name"), all.x = TRUE)
    
    res[, active_ints := apply(.SD, 1, function(row) {
      ints <- int_names[which(as.logical(row))]
      if(length(ints) == 0) "None" else paste(ints, collapse = ", ")
    }), .SDcols = patterns("active_int_")]
    
    return(res)
  }
  
  
  # This is the processed data for the analysis (economic evaluation, ICER, NMB,etc.)
processed_data <- reactive({
    dt <- copy(data_tza)
    
    # Unit costs from UI
    u_costs <- c(
      LSM = input$u_LSM, IPTSc = input$u_IPTSc, IRS = input$u_IRS, 
      Vaccine = input$u_Vaccine, ICCM = input$u_ICCM, CM = input$u_CM, 
      STD_Nets = input$u_STD_Nets, PBO_Nets = input$u_PBO_Nets, 
      IG2_Nets = input$u_IG2_Nets, PMC = input$u_PMC, SMC = input$u_SMC
    )
    
    
    dt[, total_cost := 0]
    for (int in int_names) {
      act_col  <- paste0("active_int_", int)
      elig_col <- paste0("eligible_int_", int)
      cov_col  <- paste0("coverage_int_", int)
      
      dt[, total_cost := total_cost +
           fifelse(is.na(get(elig_col)), 0,
                   nHost * get(act_col) * get(cov_col) * u_costs[int] * as.numeric(get(elig_col)))]
    }
    
    
    
    # Aggregate to district-scenario level
    ds <- dt[age_group == "0-100", .(
      cost = sum(total_cost),
      cases = sum(cum_nUncomp + cum_nSevere)
    ), by = .(admin_2, scenario_name, plan, EIR_CI)]
    
    # Reference comparison
    ref_dt <- ds[plan == input$ref_plan]
    ref_dt <- ref_dt[, .SD[1], by = .(admin_2, EIR_CI)][
      , .(admin_2, EIR_CI, ref_cost = cost, ref_cases = cases)]
    
    ds <- merge(ds, ref_dt, by = c("admin_2", "EIR_CI"), all.x = TRUE)
    
    # Cost Effective Analysis metrics
    ds[, `:=`(
      cost_diff = cost - ref_cost,
      cases_averted = ref_cases - cases,
      ICER = (cost - ref_cost) / (ref_cases - cases),
      NMB = (ref_cases - cases) * input$wtp - (cost - ref_cost),
      is.CE = ((ref_cases - cases) * input$wtp - (cost - ref_cost)) > 0
    )]
    
    return(ds)
  })
  

# CHECK ELIGIBILITY: This makes sure that all active interventions should  be eligible

check_eligibility <- function(scenario_name_val) {
  int_status <- unique(data_tza[
    scenario_name == scenario_name_val & age_group == "0-100", 
    .SD, 
    .SDcols = c("admin_2", paste0("active_int_", int_names), 
                paste0("eligible_int_", int_names))
  ])
  
  # Check if all active interventions are eligible
  int_status[, all_eligible := TRUE]
  for (int in int_names) {
    active_col <- paste0("active_int_", int)
    eligible_col <- paste0("eligible_int_", int)
    int_status[get(active_col) == TRUE & get(eligible_col) == FALSE, 
               all_eligible := FALSE]
  }
  
  return(all(int_status$all_eligible))
}


winners <- reactive({
  # Used EIR_mean as the representative data 
  ds <- processed_data()[EIR_CI == "EIR_mean"]
  
  # Budget Calculations 
  budget_curr <- ds[plan == input$ref_plan, sum(cost, na.rm = TRUE)]
  budget_env  <- budget_curr * (1 + (input$budget_inc / 100))
  
  # Identify Evaluation Plan (example; if Ref is BAU, Eval is NSP)
  eval_plan_name <- ifelse(input$ref_plan == "BAU", "NSP", "BAU")
  
  
  # Finding the most Cost-Effective Plan (Highest NMB per District)
  
  # For every district, we pick the best cost effective scenario
  p_ce <- ds[is.CE == TRUE & plan == eval_plan_name][order(admin_2, -NMB), .SD[1], by = admin_2]
  
  # If no scenario is CE for a district, it stays on Reference (BAU)
  if(nrow(p_ce) < length(unique(ds$admin_2))) {
    missing_districts <- setdiff(unique(ds$admin_2), p_ce$admin_2)
    p_ce <- rbind(p_ce, ds[plan == input$ref_plan & admin_2 %in% missing_districts])
  }
  
  
  # Optimal Assessment ( This extracts the highest health gain per District)
  p_opt <- ds[plan == eval_plan_name][order(admin_2, -cases_averted), .SD[1], by = admin_2]
  
  
  # ID Optimal Allocation (Varying Budget)
  
  # Rank scenarios (Cases Averted per Dollar)
  ds[, efficiency := cases_averted / cost_diff]
  
  # Pick the most efficient scenarios that fit the total National Budget Envelope
  p_vary <- ds[plan == eval_plan_name][order(-efficiency)]
  p_vary[, cum_cost := cumsum(cost)]
  p_vary <- p_vary[cum_cost <= budget_env]
  list(ce = p_ce, opt = p_opt, vary = p_vary, curr = budget_curr, env = budget_env)
})


render_map <- function(win_plan) {
  req(nrow(win_plan) > 0)
  
  #  Get Referencing a  given plan 
  
  ref_data <- processed_data()[plan == input$ref_plan & EIR_CI == "EIR_mean"]
  ref_names <- ref_data[, .SD[1], by = admin_2][, .(admin_2, scenario_name)]
  
  ref_ints <- get_int_names(ref_names)
  win_ints <- get_int_names(win_plan)
  
  
  ref_ints[, n_active := rowSums(.SD), .SDcols = patterns("active_int_")]
  win_ints[, n_active_win := rowSums(.SD), .SDcols = patterns("active_int_")]
  
  map_data <- merge(ref_ints[, .(admin_2, n_active, active_ints)], 
                    win_ints[, .(admin_2, n_active_win, active_ints_win = active_ints)], 
                    by = "admin_2")
  
  # This is to show when an intervention is (Added, Removed, or Modified ) in a district
  map_data[, change := fcase(
    active_ints == active_ints_win, "No Change",
    n_active_win > n_active, "Added Interventions",
    n_active_win < n_active, "Removed Interventions",
    active_ints != active_ints_win, "Mixed Interventions",
    default = "No Change"
  )]
  
  joined_sf <- merge(shape_file_tza, map_data, by = "admin_2")
  
  pal <- colorFactor(
    palette = c("#2ca25f", "#FFFF00", "#de2d26", "#808080"), 
    levels = c("Added Interventions", "Mixed Interventions", "Removed Interventions", "No Change")
  )
  
  leaflet(joined_sf) %>% addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(fillColor = ~pal(change), weight = 1, color = "white", fillOpacity = 0.9,
                label = ~paste0(admin_2, " | New Plan: ", active_ints_win)) %>%
    addLegend(pal = pal, values = ~change, title = "Policy Shift")
}


# Output variables 


# map 1 

output$map_ce  <- renderLeaflet({ render_map(winners()$ce) })



#Map 3: FACETED by interventions
output$map_varying <- renderPlot({
  win_details <- get_int_names(winners()$vary)
  
  long_data <- melt(
    win_details,
    id.vars = "admin_2",
    measure.vars = patterns("active_int_")
  )
  long_data[, variable := gsub("active_int_", "", variable)]
  
  map_sf <- merge(shape_file_tza, long_data, by = "admin_2")
  
  ggplot(map_sf) +
    geom_sf(aes(fill = value), color = "white", size = 0.2) +
    facet_wrap(~variable, ncol = 4) +
    scale_fill_manual(
      values = c("TRUE" = "#2b8cbe", "FALSE" = "#f0f0f0"),
      name = "Active"
    ) +
    theme_void() +
    theme(strip.text = element_text(size = 11, face = "bold")) +
    labs(subtitle = "Maximizing health outcomes within budget envelope")
})
 

output$table_cea <- renderDT({
  datatable(
    get_int_names(processed_data()[EIR_CI == "EIR_mean"])[, .(
      admin_2, scenario_name, active_ints, NMB, ICER, is.CE
    )]
  ) %>%
    formatCurrency('NMB', "$") %>% formatRound('ICER', 2)
})




 
}






shinyApp(ui, server)






















































