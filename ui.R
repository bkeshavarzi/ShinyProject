library(shiny)
library(shinydashboard)
library(leaflet)

shinyUI(dashboardPage(
  dashboardHeader(title = "Long Term Pavement Performance (LTPP)",titleWidth = "420px"),
  dashboardSidebar(
    
    sidebarUserPanel("Data",image="https://pavementinteractive.org/wp-content/uploads/2008/07/Hma1.jpg"),
    sidebarMenu(
      menuItem("Pavement Sections Database", tabName = "section_data"  , icon = icon("database")),
      menuItem("Temperature"  , tabName = "temperature_data", icon = icon("thermometer-0")),
      menuItem("Traffic",tabName = 'traffic_data',icon = icon('truck')),
      menuItem('Performance',tabName='performance_data',icon=icon('chain-broken')),
      menuItem('Fatigue Cracking',tabName = 'fatigue_data',icon=icon('house-damage'))
    )),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
       tabItem(tabName = "section_data",
           
              
              fluidRow(box(selectInput("state_section_data","State :",state_list,selected =state_list[1])),
                       box(selectInput("layer_type_data","Layer Type :",layer_type_list,selected = layer_type_list[1]))),
              
              fluidRow(column(4,sliderInput("thickness_histogram_slider",label=h3('Number of Bins for Thickness'),min=2,max=30,value=15)),
                       column(4,sliderInput("duration_histogram_slider",label=h3('Number of Bins for Life'),min=2,max=20,value=10)),
                       column(4,sliderInput("elevation_histogram_slider",label=h3('Number of Bins for Elevation'),min=2,max=20,value=10))),
              
              fluidRow(box(plotOutput("thickness_histogram")),box(plotOutput("duration_histogram")),box(plotOutput('elevation_histogram')),box(leafletOutput('section_map'))),
              
              fluidRow(column(6,DT::dataTableOutput("thickness_table")),
                       column(6,DT::dataTableOutput("life_table")))),
      
      tabItem(tabName = "temperature_data",
              
              fluidRow(column(2,selectInput("temperature_state","State :",temperature_state_list,selected = temperature_state_list[1],multiple = FALSE)),
                       column(2,selectInput("temperature_year","Year :",temperature_year_list,selected = temperature_year_list[1],multiple = FALSE)),
                       column(2,selectInput("temperature_month","Month :",temperature_month_list,selected=temperature_month_list[1],multiple=FALSE))),
              
              fluidRow(column(6,plotOutput("ave_temperature")),column(6,plotOutput('ave_temperature_year'))),
              fluidRow(column(6,plotOutput('ave_temp_shrp_month')),column(6,leafletOutput('ave_temperature_location'))),
              fluidRow(column(6,DT::dataTableOutput("temperature_table1")),
                       column(6,DT::dataTableOutput("temperature_table2")))),

      
      tabItem(tabName = 'traffic_data',
              
              fluidRow(column(2,selectInput('ESAL_state','State :',traffic_state,selected=traffic_state[1])),
                       column(2,selectInput('ESAL_year','Year :',traffic_year,selected = traffic_year[1]))),
              
              fluidRow(column(4,plotOutput("ESAL_year_plot")),column(4,plotOutput("ESAL_year_SHRP"))),
              fluidRow(column(4,leafletOutput("ESAL_map")),column(4,DT::dataTableOutput('ESAL_table')))),
                       
                       #box(selectInput('traffic_year','Year :',traffic_year,selected = traffic_year[1],multiple = TRUE)))),
      
      tabItem(tabName = 'performance_data',
              
              fluidRow(column(6,box(selectInput('IRI_state_1','Select State :',IRI_state,selected=IRI_state[1]))),
                       column(6,box(selectInput('IRI_shrp','Select SHRP ID:',IRI_shrp,selected=IRI_shrp[1])))),
              
              fluidRow(column(12,box(plotOutput('IRI_time_plot')))),
              
              
              fluidRow(column(6,box(selectInput('IRI_state_2','Select State :',IRI_state,selected=IRI_state[1])))),
              
              
              fluidRow(column(12,box(plotOutput('IRI_temp_plot')))),
              fluidRow(column(12,box(plotOutput('IRI_traffic_plot')))),
                       
              fluidRow(column(12,box(plotOutput('IRI_Ac_thickness_plot')))),
                       
              fluidRow(column(12,box(plotOutput("IRI_Tave_plot")))),
              fluidRow(column