

library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
# User Interface
ui <- dashboardPage(
  dashboardHeader(title = "Malaria Economic Evaluation", titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("dashboard")),
      menuItem("Budget Planner", tabName = "planner", icon = icon("calculator")),
      hr(),
      
      # Policy Inputs
      numericInput("wtp", "Willingness to Pay (per case averted, USD)", 5, min = 0),
      sliderInput("years", "Simulation Years", min = 2026, max = 2030, value = c(2026, 2030), sep = ""),
      sliderInput("budget_adj", "Budget Change (%)", -30, 30, 0),
      
      # Reference selector: counterfactual benchmark for incremental comparison
      # Optimizer searches ALL 384 scenarios and compares winner against this reference
      selectInput("ref_plan", "Select Reference Plan",
                  choices  = c("Current Practice (BAU)" = "BAU",
                               "National Strategic Plan (NSP)" = "NSP"),
                  selected = "BAU"),
      helpText("The selected plan is the counterfactual benchmark. The optimizer searches
                all 384 intervention combinations and measures gains relative to this reference.",
               style = "font-size: 11px; color: #aaa; margin-top: -8px;"),
      
      hr(),
      # Unit Costs moved to a collapsible menu item (hidden by default from main flow)
      menuItem("Assumptions: Unit Costs (USD)", icon = icon("dollar-sign"),
               numericInput("u_CM", "Case Management (CM)", value = 3.5, min = 0),
               numericInput("u_ICCM", "iCCM", value = 2.0, min = 0),
               numericInput("u_SMC", "SMC", value = 1.2, min = 0),
               numericInput("u_PMC", "PMC", value = 1.0, min = 0),
               numericInput("u_IRS", "IRS", value = 5.0, min = 0),
               numericInput("u_LSM", "LSM", value = 1.5, min = 0),
               numericInput("u_Vaccine", "Vaccine", value = 8.0, min = 0),
               numericInput("u_IPTSc", "IPTSc", value = 0.8, min = 0),
               numericInput("u_STD_Nets", "STD Nets", value = 2.5, min = 0),
               numericInput("u_PBO_Nets", "PBO Nets", value = 3.5, min = 0),
               numericInput("u_IG2_Nets", "IG2 Nets", value = 4.5, min = 0)
      )
    )
  ),
  
  dashboardBody(
    
    # Custom CSS for budget delta box
    tags$head(tags$style(HTML("
      .budget-delta-positive { background-color: #27ae60 !important; color: white !important; }
      .budget-delta-negative { background-color: #e74c3c !important; color: white !important; }
      .budget-delta-neutral  { background-color: #7f8c8d !important; color: white !important; }
      .legend-note { font-size: 11px; color: #555; font-style: italic; margin-top: 4px; }
    "))),
    
    tabItems(
      # DASHBOARD TAB 
      tabItem(tabName = "dash",
              
              # ROW 1: BUDGET OUTPUTS — now 3 boxes for clearer budget intuition
              fluidRow(
                valueBoxOutput("box_budget_ref",     width = 4),
                valueBoxOutput("box_budget_env",     width = 4),
                valueBoxOutput("box_budget_delta",   width = 4)
              ),
              
              # ROW 1b: HEALTH IMPACT OUTPUTS
              fluidRow(
                valueBoxOutput("box_cases_ref", width = 6),
                valueBoxOutput("box_cases_opt", width = 6)
              ),
              
              # ROW 2: MAPS 1 & 2
              fluidRow(
                box(title = "1) Most Cost-Effective Plan per District", width = 6, solidHeader = TRUE, status = "success",
                    p("Shows the optimal intervention combination per district that maximises Net Monetary Benefit (NMB) — i.e., health gains valued at your chosen WTP minus the additional cost over the reference plan.",
                      style = "font-size: 13px; color: #666; font-style: italic;"),
                    # Legend note below the map
                    div(
                      HTML("
                        <b style='color:#756bb1;'>&#9632;</b> <b>Reference:</b> Optimizer selected the reference plan — no better alternative fits the budget.<br>
                        <b style='color:#2ca25f;'>&#9632;</b> <b>Added:</b> Optimal plan has <i>more</i> interventions than the reference.<br>
                        <b style='color:#e67e22;'>&#9632;</b> <b>Reduced:</b> Optimal plan has <i>fewer</i> interventions than the reference.<br>
                        <b style='color:#2980b9;'>&#9632;</b> <b>Substituted:</b> Optimal plan has a <i>different mix</i> of interventions at similar count.
                      "),
                      class = "legend-note", style = "margin: 6px 10px 4px 10px;"
                    ),
                    leafletOutput("map_ce", height = 480)),
                
                box(title = "2) Optimal Plan for Maximum Health Impact", width = 6, solidHeader = TRUE, status = "primary",
                    p("Identifies which intervention combination maximises total cases averted per district within the current budget — prioritising health outcomes over cost-effectiveness ratios.",
                      style = "font-size: 13px; color: #666; font-style: italic;"),
                    div(
                      HTML("
                        <b style='color:#756bb1;'>&#9632;</b> <b>Reference:</b> Optimizer selected the reference plan — no better alternative fits the budget.<br>
                        <b style='color:#2ca25f;'>&#9632;</b> <b>Added:</b> Optimal plan has <i>more</i> interventions than the reference.<br>
                        <b style='color:#e67e22;'>&#9632;</b> <b>Reduced:</b> Optimal plan has <i>fewer</i> interventions than the reference.<br>
                        <b style='color:#2980b9;'>&#9632;</b> <b>Substituted:</b> Optimal plan has a <i>different mix</i> of interventions at similar count.
                      "),
                      class = "legend-note", style = "margin: 6px 10px 4px 10px;"
                    ),
                    leafletOutput("map_opt_assess", height = 480))
              ),
              
              # ROW 3: FACET MAP
              fluidRow(
                box(title = "3) Optimal Allocation for Varying Budget", width = 12, solidHeader = TRUE, status = "warning",
                    p("Reveals the strategic national footprint of intervention groups optimized for the specified budget envelope.",
                      style = "font-size: 14px; color: #666; font-weight: bold;"),
                    plotOutput("map_facets", height = 800))
              )
      ),
      
      # BUDGET PLANNER TAB
      tabItem(tabName = "planner",
              fluidRow(
                box(title = "Budget Simulation Settings", width = 4, status = "warning", solidHeader = TRUE,
                    p("Enter a specific budget amount to see the optimal distribution of interventions across all possible combinations."),
                    numericInput("user_budget_amount", "Total National Budget (USD):",
                                 value = 4600000000, min = 0, step = 100000000),
                    actionButton("run_planner", "Run Optimization", icon = icon("play"), class = "btn-block btn-warning")
                ),
                valueBoxOutput("planner_health_box", width = 4),
                valueBoxOutput("planner_cost_box",   width = 4)
              ),
              
              fluidRow(
                box(title = "Custom Strategic Allocation Map", width = 12, solidHeader = TRUE, status = "primary",
                    p("This map shows the optimal intervention combination per district to maximise health within your entered budget. Hover over a district to see the specific interventions deployed."),
                    # Inline legend explanation
                    div(
                      HTML("
                        <b style='color:#756bb1;'>&#9632;</b> <b>Reference:</b> Reference plan retained — no better alternative fits the budget.<br>
                        <b style='color:#2ca25f;'>&#9632;</b> <b>Added:</b> Optimal plan deploys <i>more</i> interventions than the reference.<br>
                        <b style='color:#e67e22;'>&#9632;</b> <b>Reduced:</b> Optimal plan deploys <i>fewer</i> interventions than the reference.<br>
                        <b style='color:#2980b9;'>&#9632;</b> <b>Substituted:</b> Optimal plan uses a <i>different mix</i> at similar intervention count.
                      "),
                      class = "legend-note", style = "margin: 0px 10px 8px 10px;"
                    ),
                    leafletOutput("map_planner", height = 580))
              )
      )
    )
  )
)