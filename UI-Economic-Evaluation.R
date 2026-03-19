

library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Malaria Economic Evaluation", titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("dashboard")),
      menuItem("Budget Planner", tabName = "planner", icon = icon("calculator")),      hr(),
      
      # Policy Inputs
      numericInput("wtp", "Willingness to Pay (per case averted, USD)", 5, min = 0),
      sliderInput("years", "Simulation Years", min = 2026, max = 2030, value = c(2026, 2030), sep = ""),
      selectInput("age_group", "Age Group",
                  choices  = c("All ages (0-100)" = "0-100",
                               "Under 5 (0-5)"    = "0-5",
                               "All"              = "all"),
                  selected = "0-100"),
      helpText("Select the age group to use for cases averted calculations.",
               style = "font-size: 11px; color: #aaa; margin-top: -8px;"),
      sliderInput("budget_adj", "Budget Change (%)", -30, 10, 0),
      helpText("Adjusts the budget envelope relative to BAU. Drives Maps 1 & 2, 
                Tornado charts, Facet Map, and Budget Sensitivity tab.",
               style = "font-size: 11px; color: #aaa; margin-top: -8px;"),
      selectInput("ref_plan", "Select Reference Plan",
                  choices  = c("Current Practice (BAU)" = "BAU",
                               "National Strategic Plan (NSP)" = "NSP"),
                  selected = "BAU"),
      helpText("The reference plan sets the health counterfactual — cases averted are measured 
                relative to this plan. The budget envelope is always anchored to BAU cost, 
                ensuring fair comparison across reference plans.",
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
    
    tags$head(tags$style(HTML("
      .legend-note { font-size: 11px; color: #555; font-style: italic; margin-top: 4px; }
    "))),
    
    tabItems(
      # --- DASHBOARD TAB ---
      tabItem(tabName = "dash",
              
              tabsetPanel(id = "dash_tabs", type = "tabs",
                          
                          # ── SUB-TAB 1: OVERVIEW ──────────────────────────────────
                          tabPanel("Overview",
                                   
                                   # ROW 1: BUDGET BOXES
                                   fluidRow(
                                     valueBoxOutput("box_budget_ref", width = 6),
                                     valueBoxOutput("box_budget_env", width = 6)
                                   ),
                                   
                                   # ROW 2: CASES AVERTED BOXES
                                   fluidRow(
                                     valueBoxOutput("box_cases_ref", width = 6),
                                     valueBoxOutput("box_cases_opt", width = 6)
                                   ),
                                   
                                   # ROW 3: MAPS 1 & 2
                                   fluidRow(
                                     box(title = "1) Most Cost-Effective Plan per District", width = 6,
                                         solidHeader = TRUE, status = "success",
                                         p("Shows the optimal intervention combination per district that maximises Net Monetary Benefit (NMB).",
                                           style = "font-size: 13px; color: #666; font-style: italic;"),
                                         div(HTML("
                          <b style='color:#756bb1;'>&#9632;</b> <b>Reference:</b> Optimizer selected the reference plan — no better alternative fits the budget.<br>
                          <b style='color:#2ca25f;'>&#9632;</b> <b>Added:</b> Optimal plan has <i>more</i> interventions than the reference.<br>
                          <b style='color:#e67e22;'>&#9632;</b> <b>Reduced:</b> Optimal plan has <i>fewer</i> interventions than the reference.<br>
                          <b style='color:#2980b9;'>&#9632;</b> <b>Substituted:</b> Optimal plan has a <i>different mix</i> of interventions at similar count.
                        "), class = "legend-note", style = "margin: 6px 10px 4px 10px;"),
                                         leafletOutput("map_ce", height = 480)),
                                     
                                     box(title = "2) Optimal Plan for Maximum Health Impact", width = 6,
                                         solidHeader = TRUE, status = "primary",
                                         p("Identifies which intervention combination maximises total cases averted per district within the current budget.",
                                           style = "font-size: 13px; color: #666; font-style: italic;"),
                                         div(HTML("
                          <b style='color:#756bb1;'>&#9632;</b> <b>Reference:</b> Optimizer selected the reference plan — no better alternative fits the budget.<br>
                          <b style='color:#2ca25f;'>&#9632;</b> <b>Added:</b> Optimal plan has <i>more</i> interventions than the reference.<br>
                          <b style='color:#e67e22;'>&#9632;</b> <b>Reduced:</b> Optimal plan has <i>fewer</i> interventions than the reference.<br>
                          <b style='color:#2980b9;'>&#9632;</b> <b>Substituted:</b> Optimal plan has a <i>different mix</i> of interventions at similar count.
                        "), class = "legend-note", style = "margin: 6px 10px 4px 10px;"),
                                         leafletOutput("map_opt_assess", height = 480))
                                   ),
                                   
                                   # ROW 4: TORNADO CHARTS
                                   fluidRow(
                                     box(title = "Cost vs Cases Averted — Optimal Allocations per District",
                                         width = 12, solidHeader = TRUE, status = "info",
                                         p("Each row shows one district's optimizer-selected intervention combination.
                           Left side (gold) = total cost in USD. Right side (purple) = cases averted vs reference.
                           Left chart driven by NMB optimizer (Map 1), right chart by health optimizer (Map 2).",
                                           style = "font-size: 13px; color: #666; font-style: italic;"),
                                         fluidRow(
                                           column(6,
                                                  p(strong("Map 1 — NMB Optimizer"), style = "text-align:center; margin-bottom:4px;"),
                                                  plotOutput("tornado_nmb", height = 420)
                                           ),
                                           column(6,
                                                  p(strong("Map 2 — Health Optimizer"), style = "text-align:center; margin-bottom:4px;"),
                                                  plotOutput("tornado_averted", height = 420)
                                           )
                                         )
                                     )
                                   ),
                                   
                                   # ROW 5: FACET MAP
                                   fluidRow(
                                     box(title = "3) Optimal Allocation at Current Budget", width = 12,
                                         solidHeader = TRUE, status = "warning",
                                         p("Reveals the strategic national footprint of intervention groups optimized for the current budget envelope.",
                                           style = "font-size: 14px; color: #666; font-weight: bold;"),
                                         plotOutput("map_facets", height = 800))
                                   )
                          ),
                          
                          # ── SUB-TAB 2: BUDGET SENSITIVITY ────────────────────────
                          tabPanel("Budget Sensitivity",
                                   
                                   fluidRow(
                                     box(title = "Budget Sensitivity — Cases Averted vs Reference",
                                         width = 12, solidHeader = TRUE, status = "warning",
                                         p(icon("info-circle"),
                                           "Adjust the", strong("Budget Change (%)"), "slider in the sidebar.",
                                           "Bars appear at every", strong("2% interval"), "from -30% to +10%.",
                                           "Red bars show budget cuts, gold is baseline (0%), blue is increases.",
                                           "The", strong("green dashed line"), "marks the efficiency frontier —
  the actual cost of the optimal plan. Cases averted only drop below this line.",
                                           strong("Click any bar"), "to update the intervention map below.",
                                           style = "font-size: 13px; color: #555; margin-bottom: 10px;"),
                                         
                                         # Interactive plotly bar chart
                                         plotlyOutput("sensitivity_bar", height = 350),
                                         
                                         hr(),
                                         
                                         # Budget label — updates on bar click
                                         uiOutput("sensitivity_budget_label"),
                                         
                                         # Facet map below — updates on bar click
                                         plotOutput("sensitivity_facet", height = 800)
                                     )
                                   )
                          )
              )
      ),
      
      # BUDGET PLANNER TAB
      tabItem(tabName = "planner",
              fluidRow(
                box(title = "Budget Simulation Settings", width = 4,
                    status = "warning", solidHeader = TRUE,
                    p("Enter a total national budget to find the optimal intervention
                       mix across all districts. The optimizer selects one scenario
                       per district to maximise total cases averted within the budget."),
                    p(HTML("<b>Reference budget (BAU):</b> $67,810,335<br>
                            <b>Minimum effective budget:</b> ~$51,913,488")),
                    numericInput("user_budget_amount", "Total National Budget (USD):",
                                 value = 67810335, min = 0, step = 1000000),
                    actionButton("run_planner", "Run Optimization",
                                 icon = icon("play"), class = "btn-block btn-warning")
                ),
                valueBoxOutput("planner_budget_box",  width = 4),
                valueBoxOutput("planner_health_box",  width = 4)
              ),
              fluidRow(
                valueBoxOutput("planner_cost_box", width = 6),
                valueBoxOutput("planner_savings_box", width = 6)
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


