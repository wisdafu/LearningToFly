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
library(plotly)
library(lubridate)
library(dplyr)

# Import all months and then create a single master sheet of all months of flight data
january <- read.table(file = "january.cleaned.csv", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
january$V20 <- NULL

february <- read.table(file = "february.cleaned.csv", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
february$V20 <- NULL

march <- read.table(file = "march.cleaned.csv", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
march$V20 <- NULL

april <- read.table(file = "april.cleaned.csv", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
april$V20 <- NULL

may <- read.table(file = "may.cleaned.csv", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
may$V20 <- NULL

june <- read.table(file = "june.cleaned.csv", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
june$V20 <- NULL

july <- read.table(file = "july.cleaned.csv", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
july$V20 <- NULL

august <- read.table(file = "august.cleaned.csv", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
august$V20 <- NULL

september <- read.table(file = "september.cleaned.csv", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
september$V20 <- NULL

october <- read.table(file = "october.cleaned.csv", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
october$V20 <- NULL

november <- read.table(file = "november.cleaned.csv", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
november$V20 <- NULL

december <- read.table(file = "december.cleaned.csv", sep = "\t", header = FALSE, stringsAsFactors = FALSE)
december$V20 <- NULL

master <- c()
master <- rbind(master, january)
master <- rbind(master, february)
master <- rbind(master, march)
master <- rbind(master, april)
master <- rbind(master, may)
master <- rbind(master, june)
master <- rbind(master, july)
master <- rbind(master, august)
master <- rbind(master, september)
master <- rbind(master, october)
master <- rbind(master, november)
master <- rbind(master, december)

headers <- c("FL_DATE","AIRLINE_ID","CARRIER","FL_NUM","ORIGIN_AIRPORT_ID","ORIGIN_CITY_NAME","DEST_AIRPORT_ID","DEST_CITY_NAME","DEP_TIME","ARR_TIME","CANCELLED","CANCELLATION_CODE","AIR_TIME","DISTANCE","CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY","SECURITY_DELAY","LATE_AIRCRAFT_DELAY")
colnames(master) <- headers
master$FL_DATE <- as.Date(master$FL_DATE, "%Y-%m-%d")

master$DEP_TIME <- sprintf("%04d", master$DEP_TIME)
master$DEP_TIME <- as.POSIXct(master$DEP_TIME,tz="","%H%M")
master$DEP_TIME <- format(master$DEP_TIME, "%H:%M")

master$ARR_TIME <- sprintf("%04d", master$ARR_TIME)
master$ARR_TIME <- as.POSIXct(master$ARR_TIME,tz="","%H%M")
master$ARR_TIME <- format(master$ARR_TIME, "%H:%M")

#
# NOTE: Airport IDs
#   - Midway = 13232
#   - O'Hare = 13930
#

# Shiny Dashboard

ui <- dashboardPage(
  
  # Create dashboard header
  dashboardHeader(title = "Project 2"),
  dashboardSidebar(
    sidebarMenu(
      # Create input selection for months
      selectInput("months", "Select a month", c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), selected = "Jan"
      ),
      radioButtons("airport", "Airport:",
                   c("O'Hare" = 13930,
                     "Midway" = 13232)
      )
    ) #end sidebarmenu
  ), #end dashboardSidebar
  
  dashboardBody(
    DT::dataTableOutput("tabl1"),
    DT::dataTableOutput("carrierArrDep"),
    DT::dataTableOutput("daysArrDep"),
    DT::dataTableOutput("hourlyDelays")
  ) #end dashboardBody
  
) #end dashboardPage

server <- function(input, output) { 
  
  # monthNum represents month number (1-12) based on month chosen from input dropdown
  monthNum <- reactive({
    match(input$months, month.abb)
  })
  
  airportID <- reactive({
    input$airport
  })
  
  
  # Displays all data filtered by month
  output$tabl1 <- DT::renderDataTable({
    
    DT::datatable(master[month(master$FL_DATE) == monthNum(),])
    
  })
  
  
  output$carrierArrDep <- DT::renderDataTable({
    #Arrivals and Departures by carrier
    tempArr <- group_by(master, CARRIER)
    tempArr <- filter(tempArr, as.numeric(format(FL_DATE, "%m")) == monthNum())  #check month
    tempArr <- filter(tempArr, ORIGIN_AIRPORT_ID == airportID())
    tempArr <- count(tempArr, "CARRIER")
    tempArr[2] <- NULL
    
    tempDep <- group_by(master, CARRIER)
    tempDep <- filter(tempDep, as.numeric(format(FL_DATE, "%m")) == monthNum())  #check month
    tempDep <- filter(tempDep, DEST_AIRPORT_ID == airportID())
    tempDep <- count(tempDep, 'CARRIER')
    names(tempArr)[2] <- 'Arrivals'
    tempArr$Departures <- tempDep$n
    
    DT::datatable(tempArr)
    
  })
  
  output$daysArrDep <- DT::renderDataTable({
    #Arrivals and Departures by days
    tempArr <- filter(master, as.numeric(format(FL_DATE, "%m")) == monthNum())  #check month
    tempArr <- filter(tempArr, ORIGIN_AIRPORT_ID == airportID())
    tempArr$Day <- weekdays(as.Date(tempArr$FL_DATE))
    tempArr <- group_by(tempArr, Day)
    tempArr <- count(tempArr, 'Day')
    tempArr[2] <- NULL
    names(tempArr)[2] <- 'Arrivals'
    
    tempDep <- filter(master, as.numeric(format(FL_DATE, "%m")) == monthNum())  #check month
    tempDep <- filter(tempDep, DEST_AIRPORT_ID == airportID())
    tempDep$Day <- weekdays(as.Date(tempDep$FL_DATE))
    tempDep <- group_by(tempDep, Day)
    tempDep <- count(tempDep, 'Day')
    tempArr$Departures <- tempDep$n
    
    DT::datatable(tempArr)
    
  })
  
  output$hourlyDelays <- DT::renderDataTable({
    #Hourly delays chart
    tempDel <- filter(master, as.numeric(format(FL_DATE, "%m")) == monthNum())  #check month
    tempDel <- filter(tempDel, ORIGIN_AIRPORT_ID == airportID())
    tempDel <- filter(tempDel, CANCELLED == 1)
    tempDel <- mutate(tempDel, HOUR = format(as.POSIXct(DEP_TIME, format="%H:%M"),"%H"))
    tempDel <- group_by(tempDel, HOUR)
    tempDel <- count(tempDel, HOUR)
    names(tempDel)[2] <- 'Delays'
    
    DT::datatable(tempDel)
    
  })
  
  
  
  
  
  
} #end server

shinyApp(ui, server)