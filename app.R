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
library(lubridate)
library(dplyr)

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
februaryFlightData <- read.table(file = "february.csv", sep = ",", header = TRUE)
februaryFlightData$X <- NULL
februaryFlightData$FL_DATE <- as.Date(februaryFlightData$FL_DATE, "%Y-%m-%d")
marchFlightData <- read.table(file = "march.csv", sep = ",", header = TRUE)
marchFlightData$X <- NULL
marchFlightData$FL_DATE <- as.Date(marchFlightData$FL_DATE, "%Y-%m-%d")
aprilFlightData <- read.table(file = "april.csv", sep = ",", header = TRUE)
aprilFlightData$X <- NULL
aprilFlightData$FL_DATE <- as.Date(aprilFlightData$FL_DATE, "%Y-%m-%d")
mayFlightData <- read.table(file = "may.csv", sep = ",", header = TRUE)
mayFlightData$X <- NULL
mayFlightData$FL_DATE <- as.Date(mayFlightData$FL_DATE, "%Y-%m-%d")
juneFlightData <- read.table(file = "june.csv", sep = ",", header = TRUE)
juneFlightData$X <- NULL
juneFlightData$FL_DATE <- as.Date(juneFlightData$FL_DATE, "%Y-%m-%d")
julyFlightData <- read.table(file = "july.csv", sep = ",", header = TRUE)
julyFlightData$X <- NULL
julyFlightData$FL_DATE <- as.Date(julyFlightData$FL_DATE, "%Y-%m-%d")
augustFlightData <- read.table(file = "august.csv", sep = ",", header = TRUE)
augustFlightData$X <- NULL
augustFlightData$FL_DATE <- as.Date(augustFlightData$FL_DATE, "%Y-%m-%d")
septemberFlightData <- read.table(file = "september.csv", sep = ",", header = TRUE)
septemberFlightData$X <- NULL
septemberFlightData$FL_DATE <- as.Date(septemberFlightData$FL_DATE, "%Y-%m-%d")
octoberFlightData <- read.table(file = "october.csv", sep = ",", header = TRUE)
octoberFlightData$X <- NULL
octoberFlightData$FL_DATE <- as.Date(octoberFlightData$FL_DATE, "%Y-%m-%d")
novemberFlightData <- read.table(file = "november.csv", sep = ",", header = TRUE)
novemberFlightData$X <- NULL
novemberFlightData$FL_DATE <- as.Date(novemberFlightData$FL_DATE, "%Y-%m-%d")
decemberFlightData <- read.table(file = "december.csv", sep = ",", header = TRUE)
decemberFlightData$X <- NULL
decemberFlightData$FL_DATE <- as.Date(decemberFlightData$FL_DATE, "%Y-%m-%d")

#
# Reformat Departure and Arrival time data
# e.g. 1    to 00:01
# e.g. 533  to 05:33
# e.g. 2232 to 22:32
#

januaryFlightData$DEP_TIME <- sprintf("%04d", januaryFlightData$DEP_TIME)
januaryFlightData$DEP_TIME <- as.POSIXct(januaryFlightData$DEP_TIME,tz="","%H%M")
januaryFlightData$DEP_TIME <- format(januaryFlightData$DEP_TIME, "%H:%M")
januaryFlightData$DEP_TIME12 <- format(strptime(januaryFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')
januaryFlightData$DEP_HOUR <- hour(ymd_hm(paste(januaryFlightData$FL_DATE, januaryFlightData$DEP_TIME)))

januaryFlightData$ARR_TIME <- sprintf("%04d", januaryFlightData$ARR_TIME)
januaryFlightData$ARR_TIME <- as.POSIXct(januaryFlightData$ARR_TIME,tz="","%H%M")
januaryFlightData$ARR_TIME <- format(januaryFlightData$ARR_TIME, "%H:%M")
januaryFlightData$ARR_TIME12 <- format(strptime(januaryFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')
januaryFlightData$ARR_HOUR <- hour(ymd_hm(paste(januaryFlightData$FL_DATE, januaryFlightData$ARR_TIME)))

februaryFlightData$DEP_TIME <- sprintf("%04d", februaryFlightData$DEP_TIME)
februaryFlightData$DEP_TIME <- as.POSIXct(februaryFlightData$DEP_TIME,tz="","%H%M")
februaryFlightData$DEP_TIME <- format(februaryFlightData$DEP_TIME, "%H:%M")
februaryFlightData$DEP_TIME12 <- format(strptime(februaryFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')

februaryFlightData$ARR_TIME <- sprintf("%04d", februaryFlightData$ARR_TIME)
februaryFlightData$ARR_TIME <- as.POSIXct(februaryFlightData$ARR_TIME,tz="","%H%M")
februaryFlightData$ARR_TIME <- format(februaryFlightData$ARR_TIME, "%H:%M")
februaryFlightData$ARR_TIME12 <- format(strptime(februaryFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')

marchFlightData$DEP_TIME <- sprintf("%04d", marchFlightData$DEP_TIME)
marchFlightData$DEP_TIME <- as.POSIXct(marchFlightData$DEP_TIME,tz="","%H%M")
marchFlightData$DEP_TIME <- format(marchFlightData$DEP_TIME, "%H:%M")
marchFlightData$DEP_TIME12 <- format(strptime(marchFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')

marchFlightData$ARR_TIME <- sprintf("%04d", marchFlightData$ARR_TIME)
marchFlightData$ARR_TIME <- as.POSIXct(marchFlightData$ARR_TIME,tz="","%H%M")
marchFlightData$ARR_TIME <- format(marchFlightData$ARR_TIME, "%H:%M")
marchFlightData$ARR_TIME12 <- format(strptime(marchFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')

aprilFlightData$DEP_TIME <- sprintf("%04d", aprilFlightData$DEP_TIME)
aprilFlightData$DEP_TIME <- as.POSIXct(aprilFlightData$DEP_TIME,tz="","%H%M")
aprilFlightData$DEP_TIME <- format(aprilFlightData$DEP_TIME, "%H:%M")
aprilFlightData$DEP_TIME12 <- format(strptime(aprilFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')

aprilFlightData$ARR_TIME <- sprintf("%04d", aprilFlightData$ARR_TIME)
aprilFlightData$ARR_TIME <- as.POSIXct(aprilFlightData$ARR_TIME,tz="","%H%M")
aprilFlightData$ARR_TIME <- format(aprilFlightData$ARR_TIME, "%H:%M")
aprilFlightData$ARR_TIME12 <- format(strptime(aprilFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')

mayFlightData$DEP_TIME <- sprintf("%04d", mayFlightData$DEP_TIME)
mayFlightData$DEP_TIME <- as.POSIXct(mayFlightData$DEP_TIME,tz="","%H%M")
mayFlightData$DEP_TIME <- format(mayFlightData$DEP_TIME, "%H:%M")
mayFlightData$DEP_TIME12 <- format(strptime(mayFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')

mayFlightData$ARR_TIME <- sprintf("%04d", mayFlightData$ARR_TIME)
mayFlightData$ARR_TIME <- as.POSIXct(mayFlightData$ARR_TIME,tz="","%H%M")
mayFlightData$ARR_TIME <- format(mayFlightData$ARR_TIME, "%H:%M")
mayFlightData$ARR_TIME12 <- format(strptime(mayFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')

juneFlightData$DEP_TIME <- sprintf("%04d", juneFlightData$DEP_TIME)
juneFlightData$DEP_TIME <- as.POSIXct(juneFlightData$DEP_TIME,tz="","%H%M")
juneFlightData$DEP_TIME <- format(juneFlightData$DEP_TIME, "%H:%M")
juneFlightData$DEP_TIME12 <- format(strptime(juneFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')

juneFlightData$ARR_TIME <- sprintf("%04d", juneFlightData$ARR_TIME)
juneFlightData$ARR_TIME <- as.POSIXct(juneFlightData$ARR_TIME,tz="","%H%M")
juneFlightData$ARR_TIME <- format(juneFlightData$ARR_TIME, "%H:%M")
juneFlightData$ARR_TIME12 <- format(strptime(juneFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')

julyFlightData$DEP_TIME <- sprintf("%04d", julyFlightData$DEP_TIME)
julyFlightData$DEP_TIME <- as.POSIXct(julyFlightData$DEP_TIME,tz="","%H%M")
julyFlightData$DEP_TIME <- format(julyFlightData$DEP_TIME, "%H:%M")
julyFlightData$DEP_TIME12 <- format(strptime(julyFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')

julyFlightData$ARR_TIME <- sprintf("%04d", julyFlightData$ARR_TIME)
julyFlightData$ARR_TIME <- as.POSIXct(julyFlightData$ARR_TIME,tz="","%H%M")
julyFlightData$ARR_TIME <- format(julyFlightData$ARR_TIME, "%H:%M")
julyFlightData$ARR_TIME12 <- format(strptime(julyFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')

augustFlightData$DEP_TIME <- sprintf("%04d", augustFlightData$DEP_TIME)
augustFlightData$DEP_TIME <- as.POSIXct(augustFlightData$DEP_TIME,tz="","%H%M")
augustFlightData$DEP_TIME <- format(augustFlightData$DEP_TIME, "%H:%M")
augustFlightData$DEP_TIME12 <- format(strptime(augustFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')

augustFlightData$ARR_TIME <- sprintf("%04d", augustFlightData$ARR_TIME)
augustFlightData$ARR_TIME <- as.POSIXct(augustFlightData$ARR_TIME,tz="","%H%M")
augustFlightData$ARR_TIME <- format(augustFlightData$ARR_TIME, "%H:%M")
augustFlightData$ARR_TIME12 <- format(strptime(augustFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')

septemberFlightData$DEP_TIME <- sprintf("%04d", septemberFlightData$DEP_TIME)
septemberFlightData$DEP_TIME <- as.POSIXct(septemberFlightData$DEP_TIME,tz="","%H%M")
septemberFlightData$DEP_TIME <- format(septemberFlightData$DEP_TIME, "%H:%M")
septemberFlightData$DEP_TIME12 <- format(strptime(septemberFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')

septemberFlightData$ARR_TIME <- sprintf("%04d", septemberFlightData$ARR_TIME)
septemberFlightData$ARR_TIME <- as.POSIXct(septemberFlightData$ARR_TIME,tz="","%H%M")
septemberFlightData$ARR_TIME <- format(septemberFlightData$ARR_TIME, "%H:%M")
septemberFlightData$ARR_TIME12 <- format(strptime(septemberFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')

octoberFlightData$DEP_TIME <- sprintf("%04d", octoberFlightData$DEP_TIME)
octoberFlightData$DEP_TIME <- as.POSIXct(octoberFlightData$DEP_TIME,tz="","%H%M")
octoberFlightData$DEP_TIME <- format(octoberFlightData$DEP_TIME, "%H:%M")
octoberFlightData$DEP_TIME12 <- format(strptime(octoberFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')

octoberFlightData$ARR_TIME <- sprintf("%04d", octoberFlightData$ARR_TIME)
octoberFlightData$ARR_TIME <- as.POSIXct(octoberFlightData$ARR_TIME,tz="","%H%M")
octoberFlightData$ARR_TIME <- format(octoberFlightData$ARR_TIME, "%H:%M")
octoberFlightData$ARR_TIME12 <- format(strptime(octoberFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')

novemberFlightData$DEP_TIME <- sprintf("%04d", novemberFlightData$DEP_TIME)
novemberFlightData$DEP_TIME <- as.POSIXct(novemberFlightData$DEP_TIME,tz="","%H%M")
novemberFlightData$DEP_TIME <- format(novemberFlightData$DEP_TIME, "%H:%M")
novemberFlightData$DEP_TIME12 <- format(strptime(novemberFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')

novemberFlightData$ARR_TIME <- sprintf("%04d", novemberFlightData$ARR_TIME)
novemberFlightData$ARR_TIME <- as.POSIXct(novemberFlightData$ARR_TIME,tz="","%H%M")
novemberFlightData$ARR_TIME <- format(novemberFlightData$ARR_TIME, "%H:%M")
novemberFlightData$ARR_TIME12 <- format(strptime(novemberFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')

decemberFlightData$DEP_TIME <- sprintf("%04d", decemberFlightData$DEP_TIME)
decemberFlightData$DEP_TIME <- as.POSIXct(decemberFlightData$DEP_TIME,tz="","%H%M")
decemberFlightData$DEP_TIME <- format(decemberFlightData$DEP_TIME, "%H:%M")
decemberFlightData$DEP_TIME12 <- format(strptime(decemberFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')

decemberFlightData$ARR_TIME <- sprintf("%04d", decemberFlightData$ARR_TIME)
decemberFlightData$ARR_TIME <- as.POSIXct(decemberFlightData$ARR_TIME,tz="","%H%M")
decemberFlightData$ARR_TIME <- format(decemberFlightData$ARR_TIME, "%H:%M")
decemberFlightData$ARR_TIME12 <- format(strptime(decemberFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')

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

# Get each day of the week for ohare
ohareDestMonday <- subset(ohareDest, weekdays(as.Date(ohareDest$FL_DATE,"%Y-%m-%d")) == "Monday")
ohareOriginMonday <- subset(ohareOrigin, weekdays(as.Date(ohareOrigin$FL_DATE,"%Y-%m-%d")) == "Monday")

ohareDestTuesday <- subset(ohareDest, weekdays(as.Date(ohareDest$FL_DATE,"%Y-%m-%d")) == "Tuesday")
ohareOriginTuesday <- subset(ohareOrigin, weekdays(as.Date(ohareOrigin$FL_DATE,"%Y-%m-%d")) == "Tuesday")

ohareDestWednesday <- subset(ohareDest, weekdays(as.Date(ohareDest$FL_DATE,"%Y-%m-%d")) == "Wednesday")
ohareOriginWednesday <- subset(ohareOrigin, weekdays(as.Date(ohareOrigin$FL_DATE,"%Y-%m-%d")) == "Wednesday")

ohareDestThursday <- subset(ohareDest, weekdays(as.Date(ohareDest$FL_DATE,"%Y-%m-%d")) == "Thursday")
ohareOriginThursday <- subset(ohareOrigin, weekdays(as.Date(ohareOrigin$FL_DATE,"%Y-%m-%d")) == "Thursday")

ohareDestFriday <- subset(ohareDest, weekdays(as.Date(ohareDest$FL_DATE,"%Y-%m-%d")) == "Friday")
ohareOriginFriday <- subset(ohareOrigin, weekdays(as.Date(ohareOrigin$FL_DATE,"%Y-%m-%d")) == "Friday")

ohareDestSaturday <- subset(ohareDest, weekdays(as.Date(ohareDest$FL_DATE,"%Y-%m-%d")) == "Saturday")
ohareOriginSaturday <- subset(ohareOrigin, weekdays(as.Date(ohareOrigin$FL_DATE,"%Y-%m-%d")) == "Saturday")

ohareDestSunday <- subset(ohareDest, weekdays(as.Date(ohareDest$FL_DATE,"%Y-%m-%d")) == "Sunday")
ohareOriginSunday <- subset(ohareOrigin, weekdays(as.Date(ohareOrigin$FL_DATE,"%Y-%m-%d")) == "Sunday")

# Midway
midwayDestMonday <- subset(midwayDest, weekdays(as.Date(midwayDest$FL_DATE,"%Y-%m-%d")) == "Monday")
midwayOriginMonday <- subset(midwayOrigin, weekdays(as.Date(midwayOrigin$FL_DATE,"%Y-%m-%d")) == "Monday")

midwayDestTuesday <- subset(midwayDest, weekdays(as.Date(midwayDest$FL_DATE,"%Y-%m-%d")) == "Tuesday")
midwayOriginTuesday <- subset(midwayOrigin, weekdays(as.Date(midwayOrigin$FL_DATE,"%Y-%m-%d")) == "Tuesday")

midwayDestWednesday <- subset(midwayDest, weekdays(as.Date(midwayDest$FL_DATE,"%Y-%m-%d")) == "Wednesday")
midwayOriginWednesday <- subset(midwayOrigin, weekdays(as.Date(midwayOrigin$FL_DATE,"%Y-%m-%d")) == "Wednesday")

midwayDestThursday <- subset(midwayDest, weekdays(as.Date(midwayDest$FL_DATE,"%Y-%m-%d")) == "Thursday")
midwayOriginThursday <- subset(midwayOrigin, weekdays(as.Date(midwayOrigin$FL_DATE,"%Y-%m-%d")) == "Thursday")

midwayDestFriday <- subset(midwayDest, weekdays(as.Date(midwayDest$FL_DATE,"%Y-%m-%d")) == "Friday")
midwayOriginFriday <- subset(midwayOrigin, weekdays(as.Date(midwayOrigin$FL_DATE,"%Y-%m-%d")) == "Friday")

midwayDestSaturday <- subset(midwayDest, weekdays(as.Date(midwayDest$FL_DATE,"%Y-%m-%d")) == "Saturday")
midwayOriginSaturday <- subset(midwayOrigin, weekdays(as.Date(midwayOrigin$FL_DATE,"%Y-%m-%d")) == "Saturday")

midwayDestSunday <- subset(midwayDest, weekdays(as.Date(midwayDest$FL_DATE,"%Y-%m-%d")) == "Sunday")
midwayOriginSunday <- subset(midwayOrigin, weekdays(as.Date(midwayOrigin$FL_DATE,"%Y-%m-%d")) == "Sunday")

# Create single file for both ohare and midway to encapsulate days of week for ohare
ohareDayData <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("Day", "Departures", "Arrivals")
colnames(ohareDayData) <- x

midwayDayData <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("Day", "Departures", "Arrivals")
colnames(midwayDayData) <- x

tempArr <- nrow(ohareDestMonday)
tempDep  <- nrow(ohareOriginMonday)
ohareDayData[nrow(ohareDayData) + 1, ] <- c( "Monday", tempDep,tempArr)

tempArr <- nrow(ohareDestTuesday)
tempDep  <- nrow(ohareOriginTuesday)
ohareDayData[nrow(ohareDayData) + 1, ] <- c( "Tuesday", tempDep,tempArr)

tempArr <- nrow(ohareDestWednesday)
tempDep  <- nrow(ohareOriginWednesday)
ohareDayData[nrow(ohareDayData) + 1, ] <- c( "Wednesday", tempDep,tempArr)

tempArr <- nrow(ohareDestThursday)
tempDep  <- nrow(ohareOriginThursday)
ohareDayData[nrow(ohareDayData) + 1, ] <- c( "Thursday", tempDep,tempArr)

tempArr <- nrow(ohareDestFriday)
tempDep  <- nrow(ohareOriginFriday)
ohareDayData[nrow(ohareDayData) + 1, ] <- c( "Friday", tempDep,tempArr)

tempArr <- nrow(ohareDestSaturday)
tempDep  <- nrow(ohareOriginSaturday)
ohareDayData[nrow(ohareDayData) + 1, ] <- c( "Saturday", tempDep,tempArr)

tempArr <- nrow(ohareDestSunday)
tempDep  <- nrow(ohareOriginSunday)
ohareDayData[nrow(ohareDayData) + 1, ] <- c( "Sunday", tempDep,tempArr)

# Midway day data
tempArr <- nrow(midwayDestMonday)
tempDep  <- nrow(midwayOriginMonday)
midwayDayData[nrow(midwayDayData) + 1, ] <- c( "Monday", tempDep,tempArr)

tempArr <- nrow(midwayDestTuesday)
tempDep  <- nrow(midwayOriginTuesday)
midwayDayData[nrow(midwayDayData) + 1, ] <- c( "Tuesday", tempDep,tempArr)

tempArr <- nrow(midwayDestWednesday)
tempDep  <- nrow(midwayOriginWednesday)
midwayDayData[nrow(midwayDayData) + 1, ] <- c( "Wednesday", tempDep,tempArr)

tempArr <- nrow(midwayDestThursday)
tempDep  <- nrow(midwayOriginThursday)
midwayDayData[nrow(midwayDayData) + 1, ] <- c( "Thursday", tempDep,tempArr)

tempArr <- nrow(midwayDestFriday)
tempDep  <- nrow(midwayOriginFriday)
midwayDayData[nrow(midwayDayData) + 1, ] <- c( "Friday", tempDep,tempArr)

tempArr <- nrow(midwayDestSaturday)
tempDep  <- nrow(midwayOriginSaturday)
midwayDayData[nrow(midwayDayData) + 1, ] <- c( "Saturday", tempDep,tempArr)

tempArr <- nrow(midwayDestSunday)
tempDep  <- nrow(midwayOriginSunday)
midwayDayData[nrow(midwayDayData) + 1, ] <- c( "Sunday", tempDep,tempArr)

#
# Shiny Dashboard
#

ui <- dashboardPage(
  # Create header and sidebar
  dashboardHeader(title = "CS 424 - Project 2"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("O'Hare", tabName = "ohareTab", icon = icon("plane")),
      menuItem("Midway", tabName = "midwayTab", icon = icon("plane")),
      menuItem("About", tabName = "aboutTab", icon = icon("info"))
    ) #end sidebarMenu
  ), #end dashboardSidebar
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "ohareTab", icon=icon("plane"),
              fluidRow( 
                box(title = "O'Hare Airline Data as Table", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("ohareAirlinesTable")
                )
              ),
              fluidRow(
                box(title = "O'Hare Airline Departure Data as Pie Chart", solidHeader = TRUE, status = "primary", width = 6, plotlyOutput("ohareAirlinesDepartPie")
                ),
                box(title = "O'Hare Airline Arrival Data as Pie Chart", solidHeader = TRUE, status = "primary", width = 6, plotlyOutput("ohareAirlinesArrivePie")
                )
              ),
              fluidRow(
                box(title = "O'Hare Day Airline Data as Table", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("ohareDayDataTable")
                )
              )
            ),
      
      tabItem(tabName = "midwayTab", icon=icon("plane"),
              fluidRow(
                box(title = "Midway Airline Data as Table", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("midwayAirlinesTable")
                )
              ),
              fluidRow(
                  box(title = "Midway Airline Departure Data as Pie Chart", solidHeader = TRUE, status = "primary", width = 6, plotlyOutput("midwayAirlinesDepartPie")
                  ),
                  box(title = "Midway Airline Arrival Data as Pie Chart", solidHeader = TRUE, status = "primary", width = 6, plotlyOutput("midwayAirlinesArrivePie")
                  )
              ),
              fluidRow(
                box(title = "Midway Day Airline Data as Table", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("midwayDayDataTable")
                )
              )
            )
      ),
      tabItem(tabName = "aboutTab", icon=icon("info")
              
      )
  )
)

server <- function(input, output) { 
  
  # Tables for all domestic airlines (bullet point 1)
  
  output$ohareAirlinesTable <- DT::renderDataTable(ohareAirlines, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE))
  output$midwayAirlinesTable <- DT::renderDataTable(midwayAirlines, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE))
  
  # Charts for all domestic airlines (bullet point 1 cont.)
  output$ohareAirlinesDepartPie <- renderPlotly({
    plot_ly(ohareAirlines, labels = ~ohareAirlines$Carrier, values = ~ohareAirlines$Departures, type = "pie") %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$ohareAirlinesArrivePie <- renderPlotly({
    plot_ly(ohareAirlines, labels = ~ohareAirlines$Carrier, values = ~ohareAirlines$Arrivals, type = "pie") %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$midwayAirlinesDepartPie <- renderPlotly({
    plot_ly(midwayAirlines, labels = ~midwayAirlines$Carrier, values = ~midwayAirlines$Departures, type = "pie") %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$midwayAirlinesArrivePie <- renderPlotly({
    plot_ly(midwayAirlines, labels = ~midwayAirlines$Carrier, values = ~midwayAirlines$Arrivals, type = "pie") %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # Tables for each day of the week for ohare and midway
  output$ohareDayDataTable <- DT::renderDataTable(ohareDayData, server = TRUE, options = list(pageLength = 7, lengthChange = FALSE, searching = FALSE))
  output$midwayDayDataTable <- DT::renderDataTable(midwayDayData, server = TRUE, options = list(pageLength = 7, lengthChange = FALSE, searching = FALSE))
  
}

shinyApp(ui, server)