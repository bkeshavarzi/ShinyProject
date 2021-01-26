library(shiny)
library(shinydashboard)
library(leaflet)

shinyUI(dashboardPage(
  dashboardHeader(title = "Long Term Pavement Performance (LTPP)",titleWidth = "420px"),
  dashboardSidebar(
    
    sidebarUserPanel("Data",image="https://pavementinteractive.org/wp-content/uploads/2008/07/Hma1.jpg"),
    sidebarMenu(
      menuItem("Pavement Life Span", tabName = "general"  , icon = icon("heart-o")),
      menuItem("Structure"  , tabName = "structure", icon = icon("building-o"))
    )),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "general",
              
             fluidRow(box(htmlOutput("ACPavements"),headerPanel("Number of Aspahlt Sections in LTPP")),box(htmlOutput("PCPavements"),headerPanel("Number of Concrete Sections in LTPP"))),
             fluidRow(box(selectInput("temperature_year_id","Year for Temperature Contour:",temperature_year_vector,selected =temperature_year_vector[1])),
                      box(selectInput("temperature_month_id","Month for Temperature Contour:",temperature_month_vector))),
             fluidRow(box(htmlOutput("AverageTemperature"), height = 'auto',width = 'auto',headerPanel("Average Temperature (F)")),
                      box(selectInput("traffic_year","Year For Traffic:",traffic_year_vector,selected = traffic_year_vector[1]),width=5)),
             fluidRow(box(htmlOutput("Traffic"),headerPanel("Traffic Volume"))),
             fluidRow(box(leafletOutput("SHRP")))),
      
      tabItem(tabName = "structure",
              
              fluidRow(box(selectInput("state1","State",state_name,selected = 'Texas',multiple = FALSE)),
                       box(selectInput("layer_type1","Type of Layer",layer_type,selected = 'AC',multiple = FALSE)),
                       box(sliderInput("bin1","Number of Bins:",min = 0,max = 10,value = 5))),
              
              fluidRow(box(plotOutput("LayerDistributation"),height='auto')),
              
              fluidRow(box(selectInput("state2","State for Traffic (AC):",traffic_state_vector)),
                       box(selectInput("SHRP","Select Your Section ID (AC):",SHRP_STATE)),
                       box(plotOutput("Traffic_year_growth"))),
              fluidRow(box(plotOutput("Traffic_AC_Thickness")))
      )
    )
  )
)
)