library(shiny)
library(shinydashboard)
library(leaflet)
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
      helpText("Select the age group",
               style = "font-size: 11px; color: #aaa; margin-top: -8px;"),
      sliderInput("budget_adj", "Budget Change (%)", -30, 30, 0),
      helpText("Adjusts the budget envelope",
               style = "font-size: 11px; color: #aaa; margin-top: -8px;"),
      selectInput("ref_plan", "Select Reference Plan",
                  choices  = c("Current Practice (BAU)" = "BAU",
                               "National Strategic Plan (NSP)" = "NSP"),
                  selected = "BAU"),
      helpText("The reference plan",
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
      #  DASHBOARD TAB 
      tabItem(tabName = "dash",
              
              tabsetPanel(id = "dash_tabs", type = "tabs",
                          
                          # SUB-TAB 1: OVERVIEW 
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
                                     box(title = "Cost vs Cases Averted",
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
                                   # fluidRow(
                                   #   box(title = "3) Optimal Allocation at Current Budget", width = 12,
                                   #       solidHeader = TRUE, status = "warning",
                                   #       p("Reveals the strategic national footprint of intervention groups optimized for the current budget envelope.",
                                   #         style = "font-size: 14px; color: #666; font-weight: bold;"),
                                   #       plotOutput("map_facets", height = 800))
                                   # )
                          ),
                          
                          # SUB-TAB 2: BUDGET SENSITIVITY 
                          tabPanel("Budget Sensitivity",
                                   
                                   fluidRow(
                                     box(title = "Budget Sensitivity — Cases Averted vs Reference- — Optimal Allocations per District",
                                         width = 12, solidHeader = TRUE, status = "warning",
                                         p(icon("info-circle"),
                                           "Adjust the", strong("Budget Change (%)"), "slider in the sidebar.",
                                           "Bars appear progressively at", strong("0%, +5%, and +10%"),
                                           "as you increase the budget. The facet map below always reflects the current slider value.",
                                           style = "font-size: 13px; color: #555; margin-bottom: 10px;"),
                                         
                                         # Bar chart on top
                                         plotOutput("sensitivity_bar", height = 300),
                                         
                                         hr(),
                                         
                                         # Budget label
                                         uiOutput("sensitivity_budget_label"),
                                         
                                         # Facet map below
                                         plotOutput("sensitivity_facet", height = 800)
                                     )
                                   )
                          )
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