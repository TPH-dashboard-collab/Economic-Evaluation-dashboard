
# UI - CEA Dashboard

ui <- dashboardPage( 
  dashboardHeader(title = "Malaria Economic Evaluation", titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("dashboard")),
      #menuItem("Maps", tabName = "maps", icon = icon("map")),
      hr(),
      
      # Policy Inputs
      numericInput("wtp", "Willingness to Pay (WTP, USD)", 120, min = 0),
      sliderInput("budget_inc", "Budget Change (%)", -30, 30, 0),
      selectInput("ref_plan", "Reference Plan", 
                  choices = c("BAU", "NSP"), 
                  selected = "BAU"),
      
      hr(),
      
      # I created a unit Costs for all theinterventions for the calculation purposes 
      menuItem("Unit Costs (USD)", icon = icon("dollar-sign"),
               numericInput("u_LSM", "LSM", 1.5, min = 0),
               numericInput("u_IPTSc", "IPTSc", 0.8, min = 0),
               numericInput("u_IRS", "IRS", 5.0, min = 0),
               numericInput("u_Vaccine", "Vaccine", 12.0, min = 0),
               numericInput("u_ICCM", "ICCM", 2.0, min = 0),
               numericInput("u_CM", "CM (Case Management)", 3.5, min = 0),
               numericInput("u_STD_Nets", "STD Nets", 2.5, min = 0),
               numericInput("u_PBO_Nets", "PBO Nets", 3.5, min = 0),
               numericInput("u_IG2_Nets", "IG2 Nets", 4.5, min = 0),
               numericInput("u_PMC", "PMC", 1.0, min = 0),
               numericInput("u_SMC", "SMC", 1.2, min = 0)
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      # Dashboard Tab
      tabItem(
        tabName = "dash",
        
        
        box(
          title = "Sub-national CEA Analytics", 
          width = 12, 
          DTOutput("table_cea")
        ),
        
        fluidRow(
          column(
            h4("1) Most Cost-Effective Plan Changes"), 
            width = 6, 
            solidHeader = TRUE,
            status = "success",
            leafletOutput("map_ce", height = 500)
          )
          
        ),
        
        fluidRow(
          box(
            title = "3) Optimal Allocation for Varying Budget (All Interventions)", 
            width = 12, 
            solidHeader = TRUE,
            status = "warning",
            plotOutput("map_varying", height = 800)
          )
        )
      )
      
    
    )
  )
)

