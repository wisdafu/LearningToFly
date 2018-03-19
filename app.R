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

# Rename headers
headers <- c("FL_DATE","AIRLINE_ID","CARRIER","FL_NUM","ORIGIN_AIRPORT_ID","ORIGIN_CITY_NAME","DEST_AIRPORT_ID","DEST_CITY_NAME","DEP_TIME","ARR_TIME","CANCELLED","CANCELLATION_CODE","AIR_TIME","DISTANCE","CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY","SECURITY_DELAY","LATE_AIRCRAFT_DELAY")
colnames(master) <- headers
master$FL_DATE <- as.Date(master$FL_DATE, "%Y-%m-%d")

# Format times
master$DEP_TIME <- sprintf("%04d", master$DEP_TIME)
master$DEP_TIME <- as.POSIXct(master$DEP_TIME,tz="","%H%M")
master$DEP_TIME <- format(master$DEP_TIME, "%H:%M")

master$ARR_TIME <- sprintf("%04d", master$ARR_TIME)
master$ARR_TIME <- as.POSIXct(master$ARR_TIME,tz="","%H%M")
master$ARR_TIME <- format(master$ARR_TIME, "%H:%M")

# Shiny Dashboard

ui <- dashboardPage(
  
  # Create dashboard header
  dashboardHeader(title = "Project 2"),
  
  # Sidebar
  dashboardSidebar(
    
    # Sidebar contains input that changes respective graphs based on what is currently selected
    sidebarMenu(
      
      # Create input selection for months
      selectInput("months", "Month:", c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), selected = "Jan"
      ),
      
      # Create input selection for arrivals and departures
      selectInput("arrDepList", label = "Arrivals or Departures:", choices = list(" " = c("Arrivals", "Departures"))
      ),
      
      # Create radio buttons to switch between O'Hare and Midway
      radioButtons("airport", "Airport:",
                   c("O'Hare" = 13930,
                     "Midway" = 13232)
      )
    ) #end sidebarmenu
  ), #end dashboardSidebar
  
  # Main content
  dashboardBody(
    fluidRow(
      box(
        title = "Airline Carrier Flight Counts as Pie", solidHeader = TRUE, status = "primary", width = 6, plotlyOutput("carrierArrDepPie")
      ),
      box(
        title = "Airline Carrier Flight Counts as Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("carrierArrDep")
      )
    ), #end fluidRow
    
    fluidRow(
      box(
        title = "Flights by day of the week as Pie", solidHeader = TRUE, status = "primary", width = 6, plotlyOutput("daysOfWeekPie")
      ),
      box(
        title = "Flights by day of the week as Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("daysArrDep")
      )
    ), #end fluidRow
    fluidRow(
      box(
        title = "Hourly Arrivals and Departures as Line", solidHeader = TRUE, status = "primary", width = 6, plotOutput("hourlyArrivalsandDeparturesLineGraph")
      ),
      box(
        title = "Hourly Arrivals and Departures Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("hourlyArrivalsandDeparturesTable")
      )
    ),
    fluidRow(
      box(
        title = "Hourly Delays Table", solidHeader = TRUE, status = "primary", width = 4, dataTableOutput("hourlyDelays")
      )
    ),
    fluidRow(
      box(
        title = "Flight count over time", solidHeader = TRUE, status = "primary", width = 6, plotlyOutput("carrierArrDepLine")
      )
    ),
    fluidRow(
      box(
        title = "15 Most Popular Destination Airports", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("top15Dest")
      ),
      box(
        title = "15 Most Popular Arrival Airports", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("top15Arr")
      )
    )
    
  ) #end dashboardBody
  
) #end dashboardPage

server <- function(input, output) { 
  
  # monthNum represents month number (1-12) based on month chosen from input dropdown
  monthNum <- reactive({
    match(input$months, month.abb)
  })
  
  # aiportID is ID of O'Hare and Midway
  airportID <- reactive({
    input$airport
  })
  
  # Focus on arrival or departure data
  arrDepChoice <- reactive ({
    input$arrDepList
  })
  
  # NOTE: when using these variables, you have to append () e.g. x == monthNum()
  
  # Master sheet -- Displays all data filtered by month
  output$tabl1 <- DT::renderDataTable({
    
    DT::datatable(master[month(master$FL_DATE) == monthNum(),])
    
  })
  
  # Carrier pie chart
  output$carrierArrDepPie <- renderPlotly({
    # Filtering data that will be displayed
    tempArr <- group_by(master, CARRIER)
    tempArr <- filter(tempArr, as.numeric(format(FL_DATE, "%m")) == monthNum())
    tempArr <- filter(tempArr, ORIGIN_AIRPORT_ID == airportID())
    tempArr <- count(tempArr, "Carrier")
    tempArr[2] <- NULL
    
    tempDep <- group_by(master, CARRIER)
    tempDep <- filter(tempDep, as.numeric(format(FL_DATE, "%m")) == monthNum())
    tempDep <- filter(tempDep, DEST_AIRPORT_ID == airportID())
    tempDep <- count(tempDep, 'Carrier')
    tempDep[2] <- NULL
    
    # Get data depending on what is currently selected (Arrival/Departure)
    pieData <- switch(input$arrDepList,
    "Arrivals" = tempArr, "Departures" = tempDep)
    
    # Create the actual plot
    plot_ly(pieData, labels = ~pieData$CARRIER, values = ~pieData$n, type = "pie") %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  output$daysOfWeekPie <- renderPlotly({
    #Arrivals and Departures by days
    tempArr <- filter(master, as.numeric(format(FL_DATE, "%m")) == monthNum())  #check month
    tempArr <- filter(tempArr, ORIGIN_AIRPORT_ID == airportID())
    tempArr$Day <- weekdays(as.Date(tempArr$FL_DATE))
    tempArr <- group_by(tempArr, Day)
    tempArr <- count(tempArr, 'Day')
    tempArr[2] <- NULL

    tempDep <- filter(master, as.numeric(format(FL_DATE, "%m")) == monthNum())  #check month
    tempDep <- filter(tempDep, DEST_AIRPORT_ID == airportID())
    tempDep$Day <- weekdays(as.Date(tempDep$FL_DATE))
    tempDep <- group_by(tempDep, Day)
    tempDep <- count(tempDep, 'Day')
    tempDep[2] <- NULL

    pieData <- switch(input$arrDepList,
                      "Arrivals" = tempArr, "Departures" = tempDep)
    
    plot_ly(pieData, labels = ~pieData$Day, values = ~pieData$n, type = "pie") %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$carrierArrDep <- DT::renderDataTable({
    #Arrivals and Departures by carrier
    tempArr <- group_by(master, CARRIER)
    tempArr <- filter(tempArr, as.numeric(format(FL_DATE, "%m")) == monthNum())
    tempArr <- filter(tempArr, ORIGIN_AIRPORT_ID == airportID())
    tempArr <- count(tempArr, "Carrier")
    tempArr[2] <- NULL
    
    tempDep <- group_by(master, CARRIER)
    tempDep <- filter(tempDep, as.numeric(format(FL_DATE, "%m")) == monthNum())
    tempDep <- filter(tempDep, DEST_AIRPORT_ID == airportID())
    tempDep <- count(tempDep, 'Carrier')
    names(tempArr)[2] <- 'Arrivals'
    tempArr$Departures <- tempDep$n
    
    DT::datatable(tempArr, options = list(pageLength = 10, lengthChange = FALSE, searching = FALSE))
    
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
  
  output$hourlyArrivalsandDeparturesTable <- DT::renderDataTable({
    #not using the same mutate filter function because then it misses some hour rows, can change this later.
    tempDel <- filter(master, as.numeric(format(FL_DATE, "%m")) == monthNum())  #check month
    tempDel <- filter(tempDel, ORIGIN_AIRPORT_ID == airportID())
    tempDel <- filter(tempDel, DEP_TIME != "NA")
    tempDel$HOUR <- hour(ymd_hm(paste(tempDel$FL_DATE, tempDel$DEP_TIME)))
    tempDel <-as.data.frame(table(factor(tempDel$HOUR, levels = 0:23)))
    names(tempDel)[1] <- "Hour"
    names(tempDel)[2] <- 'Departures'
    
    tempDel2 <- filter(master, as.numeric(format(FL_DATE, "%m")) == monthNum())  #check month
    tempDel2 <- filter(tempDel2, ORIGIN_AIRPORT_ID == airportID())
    tempDel2 <- filter(tempDel2, ARR_TIME != "NA")
    tempDel2$HOUR <- hour(ymd_hm(paste(tempDel2$FL_DATE, tempDel2$ARR_TIME)))
    tempDel2 <-as.data.frame(table(factor(tempDel2$HOUR, levels = 0:23)))
    names(tempDel2)[1] <- "Hour"
    names(tempDel2)[2] <- 'Arrivals'
    
    tempDel$Arrivals <- tempDel2$Arrivals
   
     DT::datatable(tempDel, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
    
  })
  
  output$hourlyArrivalsandDeparturesLineGraph <- renderPlot({
    tempDel <- filter(master, as.numeric(format(FL_DATE, "%m")) == monthNum())  #check month
    tempDel <- filter(tempDel, ORIGIN_AIRPORT_ID == airportID())
    tempDel <- filter(tempDel, DEP_TIME != "NA")
    tempDel$HOUR <- hour(ymd_hm(paste(tempDel$FL_DATE, tempDel$DEP_TIME)))
    tempDel <-as.data.frame(table(factor(tempDel$HOUR, levels = 0:23)))
    names(tempDel)[1] <- "Hour"
    names(tempDel)[2] <- 'Departures'
    
    tempDel2 <- filter(master, as.numeric(format(FL_DATE, "%m")) == monthNum())  #check month
    tempDel2 <- filter(tempDel2, ORIGIN_AIRPORT_ID == airportID())
    tempDel2 <- filter(tempDel2, ARR_TIME != "NA")
    tempDel2$HOUR <- hour(ymd_hm(paste(tempDel2$FL_DATE, tempDel2$ARR_TIME)))
    tempDel2 <-as.data.frame(table(factor(tempDel2$HOUR, levels = 0:23)))
    names(tempDel2)[1] <- "Hour"
    names(tempDel2)[2] <- 'Arrivals'
    
    tempDel$Arrivals <- tempDel2$Arrivals
    
    ggplot(tempDel, aes(x=tempDel$Hour, group = 1)) + geom_line(aes(y=tempDel$Arrivals, color = "Arrivals")) +
     geom_line(aes(y=tempDel$Departures, color = "Departures")) +  labs(x="Hour", y="Number of Flights") +
     scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
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
  
  output$top15Dest <- DT::renderDataTable({
    #15 Arrival and Departure Airports
    temp <- filter(master, as.numeric(format(FL_DATE, "%m")) == monthNum())  #check month
    temp <- filter(temp, ORIGIN_AIRPORT_ID == airportID())  #Checking Popular Destinations
    temp <- group_by(temp, DEST_AIRPORT_ID)
    temp15 <- count(temp, DEST_AIRPORT_ID)
    names(temp15)[2] <- 'Destinations'
    names(temp15)[1] <- 'Destination Airports'
    temp15 <- ungroup(temp15)
    temp15 <- arrange(temp15, desc(Destinations))
    
    DT::datatable(temp15[1:15,])
    
  })
  
  output$top15Arr <- DT::renderDataTable({
    #15 Arrival and Departure Airports
    temp <- filter(master, as.numeric(format(FL_DATE, "%m")) == monthNum())  #check month
    temp <- filter(temp, DEST_AIRPORT_ID == airportID())  #Checking Popular Destinations
    temp <- group_by(temp, ORIGIN_AIRPORT_ID)
    temp15 <- count(temp, ORIGIN_AIRPORT_ID)
    names(temp15)[2] <- 'Arrivals'
    names(temp15)[1] <- 'Arrival Airports'
    temp15 <- ungroup(temp15)
    temp15 <- arrange(temp15, desc(Arrivals))
    
    DT::datatable(temp15[1:15,])
    
  })
  
  # Grade B Plots
  
  
  # TODO: Create subset of data that has count for each day (e.g. 200 arrival flights on 2017-01-22)
  #       Then, plot data as a line graph from 2017-01-01 to 2017-12-31
  
  output$carrierArrDepLine <- renderPlotly({
    # Filtering data that will be displayed
    tempArr <- group_by(master, FL_DATE)
    tempArr <- filter(tempArr, ORIGIN_AIRPORT_ID == airportID())
    tempArr[2] <- NULL
    
    tempDep <- group_by(master, FL_DATE)
    tempDep <- filter(tempDep, DEST_AIRPORT_ID == airportID())
    tempDep[2] <- NULL
    
    # Get data depending on what is currently selected (Arrival/Departure)
    pieData <- switch(input$arrDepList,
                      "Arrivals" = tempArr, "Departures" = tempDep)
    
    # TODO" $DISTANCE is obvi wrong here, just wanted to make sure the graph worked.
    plot_ly(pieData, x = ~pieData$FL_DATE, y = ~pieData$DISTANCE, type = 'scatter', mode = 'lines')
  })
  
} #end server

shinyApp(ui, server)