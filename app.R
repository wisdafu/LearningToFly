#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#   Aaron Struck - CS424U
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
    )
  ),
  dashboardBody(
    
    )
    )

server <- function(input, output) { 
  
}

shinyApp(ui, server)