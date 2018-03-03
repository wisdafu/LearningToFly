#   
#   CS 424 Spring 2018 UIC
#   Project 2 - Learning to Fly
#
#   Aaron Struck    - Undergraduate
#   Chris Janowski  - Undergraduate
#   Steve Stranczek - Undergraduate

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
# Verify that flight date is type date and not string
#

januaryFlightData <- read.table(file = "january.csv", sep = ",", header = TRUE)
januaryFlightData$X <- NULL
januaryFlightData$FL_DATE <- as.Date(januaryFlightData$FL_DATE, "%Y-%m-%d")

#
# NOTE: Airport IDs
#   - Midway = 13232
#   - O'Hare = 13930
#

# Get subsets for midway and o'hare
midwayDest <- subset(januaryFlightData, januaryFlightData$DEST_AIRPORT_ID == "13232")
midwayOrigin <- subset(januaryFlightData, januaryFlightData$ORIGIN_AIRPORT_ID == "13232")

ohareDest <- subset(januaryFlightData, januaryFlightData$DEST_AIRPORT_ID == "13930")
ohareOrigin <- subset(januaryFlightData, januaryFlightData$ORIGIN_AIRPORT_ID == "13930")

#
# Get list and counts of all unique airlines in midway
# and o'hare, then do some column reformatting
#
# Arrival = Dest, Departure = Origin
#
ohareAirlines <- as.data.frame(table(ohareDest$CARRIER))
colnames(ohareAirlines)[colnames(ohareAirlines) == "Var1"] <- "Carrier"
colnames(ohareAirlines)[colnames(ohareAirlines) == "Freq"] <- "Arrivals"
ohareAirlines$Departures <- table(ohareOrigin$CARRIER)

midwayAirlines <- as.data.frame(table(midwayDest$CARRIER))
colnames(midwayAirlines)[colnames(midwayAirlines) == "Var1"] <- "Carrier"
colnames(midwayAirlines)[colnames(midwayAirlines) == "Freq"] <- "Arrivals"
midwayAirlines$Departures <- table(midwayOrigin$CARRIER)


# Get total arrivals and departures for each airport
midwayDestTotal <- nrow(midwayDest)
midwayOriginTotal <- nrow(midwayOrigin)

ohareDestTotal <- nrow(ohareDest)
ohareOriginTotal <- nrow(ohareOrigin)

# TODO: Get each day of the week
weekdays(as.Date('2017-01-01','%Y-%m-%d'))
ohareDestMonday <- subset(ohareDest, weekdays(as.Date(ohareDest$FL_DATE,"%Y-%m-%d")) == "Monday")
ohareDestTuesday <- subset(ohareDest, weekdays(as.Date(ohareDest$FL_DATE,"%Y-%m-%d")) == "Tuesday")
ohareDestWednesday <- subset(ohareDest, weekdays(as.Date(ohareDest$FL_DATE,"%Y-%m-%d")) == "Wednesday")
ohareDestThursday <- subset(ohareDest, weekdays(as.Date(ohareDest$FL_DATE,"%Y-%m-%d")) == "Thursday")
ohareDestFriday <- subset(ohareDest, weekdays(as.Date(ohareDest$FL_DATE,"%Y-%m-%d")) == "Friday")
ohareDestSaturday <- subset(ohareDest, weekdays(as.Date(ohareDest$FL_DATE,"%Y-%m-%d")) == "Saturday")
ohareDestSunday <- subset(ohareDest, weekdays(as.Date(ohareDest$FL_DATE,"%Y-%m-%d")) == "Sunday")
  
#
# Shiny Dashboard
#

ui <- dashboardPage(
  # Create header and sidebar
  dashboardHeader(title = "CS 424 - Project 2"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main", tabName = "mainTab", icon = icon("dashboard"))
    ) #end sidebarMenu
  ), #end dashboardSidebar
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "mainTab", icon=icon("home"),
              fluidRow( 
              )
      )
    )
  )
)

server <- function(input, output) { 
  
}

shinyApp(ui, server)