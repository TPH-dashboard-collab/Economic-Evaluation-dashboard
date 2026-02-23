
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
# User interface 
ui <- dashboardPage(
  dashboardHeader(title = "Malaria Economic Evaluation", titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("dashboard")),
      hr(),
      numericInput("wtp", "Willingness to Pay (per case averted, USD)", 120, min = 0),
      sliderInput("years", "Simulation Years", min = 2026, max = 2030, value = c(2026, 2030), sep = ""),
      sliderInput("budget_adj", "Budget Change (%)", -30, 30, 0),
      
      #ONLY ONE SELECTOR NEEDED
      # If user picks BAU, the server automatically optimisations for NSP
      selectInput("ref_plan", "Reference Plan (Baseline)", choices = c("BAU", "NSP"), selected = "BAU"),
      
      hr(), 
      menuItem("Unit Costs (USD)", icon = icon("dollar-sign"),
               numericInput("u_CM", "Case Management (CM)", value = 0.5, min = 0),
               numericInput("u_ICCM", "iCCM", value = 1.0, min = 0),
               numericInput("u_SMC", "SMC", value = 1., min = 0),
               numericInput("u_PMC", "PMC", value = 1.0, min = 0),
               numericInput("u_IRS", "IRS", value = 0.5, min = 0),
               numericInput("u_LSM", "LSM", value = 0.3, min = 0),
               numericInput("u_Vaccine", "Vaccine", value = 0.6, min = 0),
               numericInput("u_IPTSc", "IPTSc", value = 0.8, min = 0),
               numericInput("u_STD_Nets", "STD Nets", value = 0.5, min = 0),
               numericInput("u_PBO_Nets", "PBO Nets", value = 1.5, min = 0),
               numericInput("u_IG2_Nets", "IG2 Nets", value = 0.5, min = 0)
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dash",
              # ROW 1: BUDGET OUTPUTS
              fluidRow(
                valueBoxOutput("box_budget_curr", width = 4),
                valueBoxOutput("box_budget_env", width = 4)
              ),
              box(
                title = "Sub-national CEA Analytics", 
                width = 12, 
                DTOutput("table_cea")
              ),
              # ROW 2: MAPS
              fluidRow(
                box(title = "1) Most Cost-Effective Plan Changes", width = 6, solidHeader = TRUE, status = "success", p("Displays the geographic shift from the baseline to the strategy maximizing economic value (Net Monetary Benefit).",
                                                                                                                        style = "font-size: 13px; color: #666; font-style: italic;"),                                                                                                
                    leafletOutput("map_ce", height = 500)),
                box(title = "2) Optimal Assessment Changes", width = 6, solidHeader = TRUE, status = "primary", p("Map 2 shows with the existing budget, we cannot afford a better strategy, we stay with the status quo (BAU).", 
                                                                                                                 style = "font-size: 14px; color: #666; font-weight: bold;"),
                    leafletOutput("map_opt", height = 500))
              ),
              
              # ROW 3: FACETS
              fluidRow(
                box(title = "3) Optimal Allocation for Varying Budget", width = 12, solidHeader = TRUE, status = "warning", p("Reveals the strategic national footprint of intervention groups optimized for the specified budget envelope.", 
                                                                                                                              style = "font-size: 14px; color: #666; font-weight: bold;"),
                    plotOutput("map_facets", height = 800))
              )
      )
    )
  )
)