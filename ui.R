library(shinydashboard)
shinyUI(dashboardPage( 
  dashboardHeader(title = "Long Term Pavement Performance Database",titleWidth = "350px"), 
  dashboardSidebar(
    sidebarUserPanel("Pavements Data",image="https://pavementinteractive.org/wp-content/uploads/2008/07/Hma1.jpg"),
    sidebarMenu(
      menuItem("Big Picture", tabName = "general",icon = icon("road")),
      menuItem("Structure", tabName = "structure",icon = icon("building-o")),
    )), 
  dashboardBody(
    tabItems(
      tabItem(tabName = "general",
              fluidRow(box(htmlOutput("ACPavements"), height = 300),box(htmlOutput("PCPavements"), height = 300)),
              fluidRow(box(selectInput("temperature_year_id","Year :",temperature_year_vector,selected =temperature_year_vector[1])),
                       box(selectInput("temperature_month_id","Month :",temperature_month_vector)),
                       box(htmlOutput("AverageTemperature"), height = 300)),
              fluidRow(box(selectInput("traffic_year","Year :",traffic_year_vector,selected = traffic_year_vector[1])),
                       box(htmlOutput("Traffic"), height = 300)),
              fluidRow(box(htmlOutput("SHRP"),height = 300))
              ),
      tabItem(tabName = "structure",
              
              fluidRow(box(selectInput("state1","State",state_name,selected = 'Texas',multiple = FALSE)),
                       box(selectInput("layer_type1","Type of Layer",layer_type,selected = 'AC',multiple = FALSE)),
                       box(sliderInput("bin1","Number of Bins:",min = 0,max = 10,value = 5))),
              
              fluidRow(box(plotOutput("LayerDistributation"),height=500)),
              
              fluidRow(box(selectInput("state2","State:",traffic_year_vector)),
                       box(selectInput("SHRP","Select Your Section ID:",SHRP_STATE)),
                       box(plotOutput("Traffic_year_growth"),height=500),
                       box(htmlOutput("Traffic_AC_Thickness",height=500)),
                       box(htmlOutput("Traffic_PC_Thickness",height=500)))
              ),
    )
  )
))
