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

#
# Import libraries
#
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(leaflet)
library(plotly)

#
# Read in data for January
# 
# For some weird reason, there exists a column called 'X' 
# with no data fields... So remove it via NULL
#
# Verify that flight date is type data and not string
#

januaryFlightData <- read.table(file = "january.csv", sep = ",", header = TRUE)
januaryFlightData$X <- NULL
januaryFlightData$FL_DATE <- as.Date(januaryFlightData$FL_DATE, "%Y-%m-%d")

#
# NOTE: Airport IDs
#   - Midway = 13232
#   - O'Hare = 13930
#

#
# Shiny Dashboard
#

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
              fluidRow( 
              )
      )
    )
    )
)

server <- function(input, output) { 
  
}

shinyApp(ui, server)