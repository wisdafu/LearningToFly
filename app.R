#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#   Aaron Struck   - CS424 - Undergraduate
#   Chris Janowski - CS424 - Undergraduate
#

## app.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)

#Shiny Dashboard
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main", tabName = "mainTab", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "mainTab",
              fluidRow( #cholera Tab
                
              )
      )
    )
    )
)

server <- function(input, output) { 
  
}

shinyApp(ui, server)