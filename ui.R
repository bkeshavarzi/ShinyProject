library(shinydashboard)
shinyUI(dashboardPage( 
  dashboardHeader(title = "Long Term Pavement Performance Database",titleWidth = "350px"), 
  dashboardSidebar(
    sidebarUserPanel("Pavements Data",image="https://pavementinteractive.org/wp-content/uploads/2008/07/Hma1.jpg"),
    sidebarMenu(
      menuItem("Big Picture", tabName = "general",icon = icon("road")),
      menuItem("Structure", tabName = "structure",icon = icon("building-o")),
      menuItem("Distress", tabName = "distress",icon = icon("house-damage"))
    )), 
  dashboardBody(
    tabItems(
      tabItem(tabName = "general",
              fluidRow(box(htmlOutput("ACPavements"), height = 300),box(htmlOutput("PCPavements"), height = 300)),
              fluidRow(box(),box(htmlOutput("AverageTemperature"), height = 300)),
              fluidRow(box(htmlOutput("Traffic"), height = 300),box(htmlOutput("KValue"), height = 300))
              ),
      tabItem(tabName = "structure","I am going to talk about pavement structue here"),
      tabItem(tabName = "distress","I am going to talk about distress here")
    )
  )
))