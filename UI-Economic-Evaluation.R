library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = "Malaria Economic Evaluation", titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("dashboard")),
      hr(),
      numericInput("wtp", "Willingness to Pay (WTP, USD)", 120, min = 0),
      sliderInput("years", "Simulation Years", min = 2026, max = 2030, value = c(2026, 2030), sep = ""),
      sliderInput("budget_adj", "Budget Change (%)", -30, 30, 0),
      
      # ONLY ONE SELECTOR NEEDED
      # If user picks BAU, the server automatically optimizations for NSP
      selectInput("ref_plan", "Reference Plan (Baseline)", choices = c("BAU", "NSP"), selected = "BAU"),
      
      hr(),
      menuItem("Unit Costs (USD)", icon = icon("dollar-sign"),
               uiOutput("cost_inputs") 
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dash",
              # ROW 1: BUDGET OUTPUTS
              fluidRow(
                valueBoxOutput("box_budget_curr", width = 6),
                valueBoxOutput("box_budget_env", width = 6)
              ),
              
              # ROW 2: MAPS
              fluidRow(
                box(title = "1) Most Cost-Effective Plan Changes", width = 6, solidHeader = TRUE, status = "success",
                    leafletOutput("map_ce", height = 500)),
                box(title = "2) Optimal Assessment Changes", width = 6, solidHeader = TRUE, status = "primary",
                    leafletOutput("map_opt_assess", height = 500))
              ),
              
              # ROW 3: FACETS
              fluidRow(
                box(title = "3) Optimal Allocation for Varying Budget", width = 12, solidHeader = TRUE, status = "warning",
                    plotOutput("map_facets", height = 800))
              )
      )
    )
  )
)