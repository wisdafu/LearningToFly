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
februaryFlightData$DEP_HOUR <- hour(ymd_hm(paste(februaryFlightData$FL_DATE, februaryFlightData$DEP_TIME)))

februaryFlightData$ARR_TIME <- sprintf("%04d", februaryFlightData$ARR_TIME)
februaryFlightData$ARR_TIME <- as.POSIXct(februaryFlightData$ARR_TIME,tz="","%H%M")
februaryFlightData$ARR_TIME <- format(februaryFlightData$ARR_TIME, "%H:%M")
februaryFlightData$ARR_TIME12 <- format(strptime(februaryFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')
februaryFlightData$ARR_HOUR <- hour(ymd_hm(paste(februaryFlightData$FL_DATE, februaryFlightData$ARR_TIME)))

marchFlightData$DEP_TIME <- sprintf("%04d", marchFlightData$DEP_TIME)
marchFlightData$DEP_TIME <- as.POSIXct(marchFlightData$DEP_TIME,tz="","%H%M")
marchFlightData$DEP_TIME <- format(marchFlightData$DEP_TIME, "%H:%M")
marchFlightData$DEP_TIME12 <- format(strptime(marchFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')
marchFlightData$DEP_HOUR <- hour(ymd_hm(paste(marchFlightData$FL_DATE, marchFlightData$DEP_TIME)))

marchFlightData$ARR_TIME <- sprintf("%04d", marchFlightData$ARR_TIME)
marchFlightData$ARR_TIME <- as.POSIXct(marchFlightData$ARR_TIME,tz="","%H%M")
marchFlightData$ARR_TIME <- format(marchFlightData$ARR_TIME, "%H:%M")
marchFlightData$ARR_TIME12 <- format(strptime(marchFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')
marchFlightData$ARR_HOUR <- hour(ymd_hm(paste(marchFlightData$FL_DATE, marchFlightData$ARR_TIME)))

aprilFlightData$DEP_TIME <- sprintf("%04d", aprilFlightData$DEP_TIME)
aprilFlightData$DEP_TIME <- as.POSIXct(aprilFlightData$DEP_TIME,tz="","%H%M")
aprilFlightData$DEP_TIME <- format(aprilFlightData$DEP_TIME, "%H:%M")
aprilFlightData$DEP_TIME12 <- format(strptime(aprilFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')
aprilFlightData$DEP_HOUR <- hour(ymd_hm(paste(aprilFlightData$FL_DATE, aprilFlightData$DEP_TIME)))

aprilFlightData$ARR_TIME <- sprintf("%04d", aprilFlightData$ARR_TIME)
aprilFlightData$ARR_TIME <- as.POSIXct(aprilFlightData$ARR_TIME,tz="","%H%M")
aprilFlightData$ARR_TIME <- format(aprilFlightData$ARR_TIME, "%H:%M")
aprilFlightData$ARR_TIME12 <- format(strptime(aprilFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')
aprilFlightData$ARR_HOUR <- hour(ymd_hm(paste(aprilFlightData$FL_DATE, aprilFlightData$ARR_TIME)))

mayFlightData$DEP_TIME <- sprintf("%04d", mayFlightData$DEP_TIME)
mayFlightData$DEP_TIME <- as.POSIXct(mayFlightData$DEP_TIME,tz="","%H%M")
mayFlightData$DEP_TIME <- format(mayFlightData$DEP_TIME, "%H:%M")
mayFlightData$DEP_TIME12 <- format(strptime(mayFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')
mayFlightData$DEP_HOUR <- hour(ymd_hm(paste(aprilFlightData$FL_DATE, mayFlightData$DEP_TIME)))

mayFlightData$ARR_TIME <- sprintf("%04d", mayFlightData$ARR_TIME)
mayFlightData$ARR_TIME <- as.POSIXct(mayFlightData$ARR_TIME,tz="","%H%M")
mayFlightData$ARR_TIME <- format(mayFlightData$ARR_TIME, "%H:%M")
mayFlightData$ARR_TIME12 <- format(strptime(mayFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')
mayFlightData$ARR_HOUR <- hour(ymd_hm(paste(aprilFlightData$FL_DATE, mayFlightData$ARR_TIME)))

juneFlightData$DEP_TIME <- sprintf("%04d", juneFlightData$DEP_TIME)
juneFlightData$DEP_TIME <- as.POSIXct(juneFlightData$DEP_TIME,tz="","%H%M")
juneFlightData$DEP_TIME <- format(juneFlightData$DEP_TIME, "%H:%M")
juneFlightData$DEP_TIME12 <- format(strptime(juneFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')
juneFlightData$DEP_HOUR <- hour(ymd_hm(paste(juneFlightData$FL_DATE, juneFlightData$DEP_TIME)))

juneFlightData$ARR_TIME <- sprintf("%04d", juneFlightData$ARR_TIME)
juneFlightData$ARR_TIME <- as.POSIXct(juneFlightData$ARR_TIME,tz="","%H%M")
juneFlightData$ARR_TIME <- format(juneFlightData$ARR_TIME, "%H:%M")
juneFlightData$ARR_TIME12 <- format(strptime(juneFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')
juneFlightData$ARR_HOUR <- hour(ymd_hm(paste(juneFlightData$FL_DATE, juneFlightData$ARR_TIME)))

julyFlightData$DEP_TIME <- sprintf("%04d", julyFlightData$DEP_TIME)
julyFlightData$DEP_TIME <- as.POSIXct(julyFlightData$DEP_TIME,tz="","%H%M")
julyFlightData$DEP_TIME <- format(julyFlightData$DEP_TIME, "%H:%M")
julyFlightData$DEP_TIME12 <- format(strptime(julyFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')
julyFlightData$DEP_HOUR <- hour(ymd_hm(paste(julyFlightData$FL_DATE, julyFlightData$DEP_TIME)))

julyFlightData$ARR_TIME <- sprintf("%04d", julyFlightData$ARR_TIME)
julyFlightData$ARR_TIME <- as.POSIXct(julyFlightData$ARR_TIME,tz="","%H%M")
julyFlightData$ARR_TIME <- format(julyFlightData$ARR_TIME, "%H:%M")
julyFlightData$ARR_TIME12 <- format(strptime(julyFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')
julyFlightData$ARR_HOUR <- hour(ymd_hm(paste(julyFlightData$FL_DATE, julyFlightData$ARR_TIME)))

augustFlightData$DEP_TIME <- sprintf("%04d", augustFlightData$DEP_TIME)
augustFlightData$DEP_TIME <- as.POSIXct(augustFlightData$DEP_TIME,tz="","%H%M")
augustFlightData$DEP_TIME <- format(augustFlightData$DEP_TIME, "%H:%M")
augustFlightData$DEP_TIME12 <- format(strptime(augustFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')
augustFlightData$DEP_HOUR <- hour(ymd_hm(paste(augustFlightData$FL_DATE, augustFlightData$DEP_TIME)))

augustFlightData$ARR_TIME <- sprintf("%04d", augustFlightData$ARR_TIME)
augustFlightData$ARR_TIME <- as.POSIXct(augustFlightData$ARR_TIME,tz="","%H%M")
augustFlightData$ARR_TIME <- format(augustFlightData$ARR_TIME, "%H:%M")
augustFlightData$ARR_TIME12 <- format(strptime(augustFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')
augustFlightData$ARR_HOUR <- hour(ymd_hm(paste(augustFlightData$FL_DATE, augustFlightData$ARR_TIME)))

septemberFlightData$DEP_TIME <- sprintf("%04d", septemberFlightData$DEP_TIME)
septemberFlightData$DEP_TIME <- as.POSIXct(septemberFlightData$DEP_TIME,tz="","%H%M")
septemberFlightData$DEP_TIME <- format(septemberFlightData$DEP_TIME, "%H:%M")
septemberFlightData$DEP_TIME12 <- format(strptime(septemberFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')
septemberFlightData$DEP_HOUR <- hour(ymd_hm(paste(septemberFlightData$FL_DATE, septemberFlightData$DEP_TIME)))

septemberFlightData$ARR_TIME <- sprintf("%04d", septemberFlightData$ARR_TIME)
septemberFlightData$ARR_TIME <- as.POSIXct(septemberFlightData$ARR_TIME,tz="","%H%M")
septemberFlightData$ARR_TIME <- format(septemberFlightData$ARR_TIME, "%H:%M")
septemberFlightData$ARR_TIME12 <- format(strptime(septemberFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')
septemberFlightData$ARR_HOUR <- hour(ymd_hm(paste(septemberFlightData$FL_DATE, septemberFlightData$ARR_TIME)))

octoberFlightData$DEP_TIME <- sprintf("%04d", octoberFlightData$DEP_TIME)
octoberFlightData$DEP_TIME <- as.POSIXct(octoberFlightData$DEP_TIME,tz="","%H%M")
octoberFlightData$DEP_TIME <- format(octoberFlightData$DEP_TIME, "%H:%M")
octoberFlightData$DEP_TIME12 <- format(strptime(octoberFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')
octoberFlightData$DEP_HOUR <- hour(ymd_hm(paste(octoberFlightData$FL_DATE, octoberFlightData$DEP_TIME)))

octoberFlightData$ARR_TIME <- sprintf("%04d", octoberFlightData$ARR_TIME)
octoberFlightData$ARR_TIME <- as.POSIXct(octoberFlightData$ARR_TIME,tz="","%H%M")
octoberFlightData$ARR_TIME <- format(octoberFlightData$ARR_TIME, "%H:%M")
octoberFlightData$ARR_TIME12 <- format(strptime(octoberFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')
octoberFlightData$ARR_HOUR <- hour(ymd_hm(paste(octoberFlightData$FL_DATE, octoberFlightData$ARR_TIME)))

novemberFlightData$DEP_TIME <- sprintf("%04d", novemberFlightData$DEP_TIME)
novemberFlightData$DEP_TIME <- as.POSIXct(novemberFlightData$DEP_TIME,tz="","%H%M")
novemberFlightData$DEP_TIME <- format(novemberFlightData$DEP_TIME, "%H:%M")
novemberFlightData$DEP_TIME12 <- format(strptime(novemberFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')
novemberFlightData$DEP_HOUR <- hour(ymd_hm(paste(novemberFlightData$FL_DATE, novemberFlightData$DEP_TIME)))

novemberFlightData$ARR_TIME <- sprintf("%04d", novemberFlightData$ARR_TIME)
novemberFlightData$ARR_TIME <- as.POSIXct(novemberFlightData$ARR_TIME,tz="","%H%M")
novemberFlightData$ARR_TIME <- format(novemberFlightData$ARR_TIME, "%H:%M")
novemberFlightData$ARR_TIME12 <- format(strptime(novemberFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')
novemberFlightData$ARR_HOUR <- hour(ymd_hm(paste(novemberFlightData$FL_DATE, novemberFlightData$ARR_TIME)))

decemberFlightData$DEP_TIME <- sprintf("%04d", decemberFlightData$DEP_TIME)
decemberFlightData$DEP_TIME <- as.POSIXct(decemberFlightData$DEP_TIME,tz="","%H%M")
decemberFlightData$DEP_TIME <- format(decemberFlightData$DEP_TIME, "%H:%M")
decemberFlightData$DEP_TIME12 <- format(strptime(decemberFlightData$DEP_TIME, format='%H:%M'), '%I:%M %p')
decemberFlightData$DEP_HOUR <- hour(ymd_hm(paste(decemberFlightData$FL_DATE, decemberFlightData$DEP_TIME)))

decemberFlightData$ARR_TIME <- sprintf("%04d", decemberFlightData$ARR_TIME)
decemberFlightData$ARR_TIME <- as.POSIXct(decemberFlightData$ARR_TIME,tz="","%H%M")
decemberFlightData$ARR_TIME <- format(decemberFlightData$ARR_TIME, "%H:%M")
decemberFlightData$ARR_TIME12 <- format(strptime(decemberFlightData$ARR_TIME, format='%H:%M'), '%I:%M %p')
decemberFlightData$ARR_HOUR <- hour(ymd_hm(paste(decemberFlightData$FL_DATE, decemberFlightData$ARR_TIME)))

#
# NOTE: Airport IDs
#   - Midway = 13232
#   - O'Hare = 13930
#

# Get subsets for midway and o'hare
midwayDest <- subset(januaryFlightData, januaryFlightData$DEST_AIRPORT_ID == "13232")
midwayOrigin <- subset(januaryFlightData, januaryFlightData$ORIGIN_AIRPORT_ID == "13232")
midwayARRHourlyData <-na.omit(as.data.frame(table(midwayDest$ARR_HOUR)))
midwayDepHourlyData <-na.omit(as.data.frame(table(midwayOrigin$DEP_HOUR)))
 
ohareDest <- subset(januaryFlightData, januaryFlightData$DEST_AIRPORT_ID == "13930")
ohareOrigin <- subset(januaryFlightData, januaryFlightData$ORIGIN_AIRPORT_ID == "13930")
ohareArrHourlyData <-na.omit(as.data.frame(table(ohareDest$ARR_HOUR)))
ohareDepHourlyData <-na.omit(as.data.frame(table(ohareOrigin$DEP_HOUR)))

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

#-------------------------------------------------------------------------------------------------------------------------------------------
# get hourly data for both airports
#jan
midwayJanArrHourlyData <- na.omit(as.data.frame(table(factor(midwayDest$ARR_HOUR, levels = 0:23))))
midwayJanDepHourlyData <- na.omit(as.data.frame(table(factor(midwayOrigin$DEP_HOUR, levels = 0:23))))

ohareJanArrHourlyData <- na.omit(as.data.frame(table(factor(ohareDest$ARR_HOUR, levels = 0:23))))
ohareJanDepHourlyData <- na.omit(as.data.frame(table(factor(ohareOrigin$DEP_HOUR, levels = 0:23))))

midwayJanHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
midwayJanHourlyData$Arr <- midwayJanArrHourlyData$Freq
midwayJanHourlyData$Dep <- midwayJanDepHourlyData$Freq

ohareJanHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
ohareJanHourlyData$Arr <- ohareJanArrHourlyData$Freq
ohareJanHourlyData$Dep <- ohareJanDepHourlyData$Freq

#feb
midwayFebArrHourlyData <- na.omit(as.data.frame(table(factor(midwayDest$ARR_HOUR, levels = 0:23))))
midwayFebDepHourlyData <- na.omit(as.data.frame(table(factor(midwayOrigin$DEP_HOUR, levels = 0:23))))

ohareFebArrHourlyData <- na.omit(as.data.frame(table(factor(ohareDest$ARR_HOUR, levels = 0:23))))
ohareFebDepHourlyData <- na.omit(as.data.frame(table(factor(ohareOrigin$DEP_HOUR, levels = 0:23))))

midwayFebHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
midwayFebHourlyData$Arr <- midwayFebArrHourlyData$Freq
midwayFebHourlyData$Dep <- midwayFebDepHourlyData$Freq

ohareFebHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
ohareFebHourlyData$Arr <- ohareFebArrHourlyData$Freq
ohareFebHourlyData$Dep <- ohareFebDepHourlyData$Freq

#march
midwayMarchArrHourlyData <- na.omit(as.data.frame(table(factor(midwayDest$ARR_HOUR, levels = 0:23))))
midwayMarchDepHourlyData <- na.omit(as.data.frame(table(factor(midwayOrigin$DEP_HOUR, levels = 0:23))))

ohareMarchArrHourlyData <- na.omit(as.data.frame(table(factor(ohareDest$ARR_HOUR, levels = 0:23))))
ohareMarchDepHourlyData <- na.omit(as.data.frame(table(factor(ohareOrigin$DEP_HOUR, levels = 0:23))))

midwayMarchHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
midwayMarchHourlyData$Arr <- midwayMarchArrHourlyData$Freq
midwayMarchHourlyData$Dep <- midwayMarchDepHourlyData$Freq

ohareMarchHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
ohareMarchHourlyData$Arr <- ohareMarchArrHourlyData$Freq
ohareMarchHourlyData$Dep <- ohareMarchDepHourlyData$Freq

#april
midwayAprilArrHourlyData <- na.omit(as.data.frame(table(factor(midwayDest$ARR_HOUR, levels = 0:23))))
midwayAprilDepHourlyData <- na.omit(as.data.frame(table(factor(midwayOrigin$DEP_HOUR, levels = 0:23))))

ohareAprilArrHourlyData <- na.omit(as.data.frame(table(factor(ohareDest$ARR_HOUR, levels = 0:23))))
ohareAprilDepHourlyData <- na.omit(as.data.frame(table(factor(ohareOrigin$DEP_HOUR, levels = 0:23))))

midwayAprilHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
midwayAprilHourlyData$Arr <- midwayAprilArrHourlyData$Freq
midwayAprilHourlyData$Dep <- midwayAprilDepHourlyData$Freq

ohareAprilHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
ohareAprilHourlyData$Arr <- ohareAprilArrHourlyData$Freq
ohareAprilHourlyData$Dep <- ohareAprilDepHourlyData$Freq

#may
midwayMayArrHourlyData <- na.omit(as.data.frame(table(factor(midwayDest$ARR_HOUR, levels = 0:23))))
midwayMayDepHourlyData <- na.omit(as.data.frame(table(factor(midwayOrigin$DEP_HOUR, levels = 0:23))))

ohareMayArrHourlyData <- na.omit(as.data.frame(table(factor(ohareDest$ARR_HOUR, levels = 0:23))))
ohareMayDepHourlyData <- na.omit(as.data.frame(table(factor(ohareOrigin$DEP_HOUR, levels = 0:23))))

midwayMayHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
midwayMayHourlyData$Arr <- midwayMayArrHourlyData$Freq
midwayMayHourlyData$Dep <- midwayMayDepHourlyData$Freq

ohareMayHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
ohareMayHourlyData$Arr <- ohareMayArrHourlyData$Freq
ohareMayHourlyData$Dep <- ohareMayDepHourlyData$Freq

#june
midwayJuneArrHourlyData <- na.omit(as.data.frame(table(factor(midwayDest$ARR_HOUR, levels = 0:23))))
midwayJuneDepHourlyData <- na.omit(as.data.frame(table(factor(midwayOrigin$DEP_HOUR, levels = 0:23))))

ohareJuneArrHourlyData <- na.omit(as.data.frame(table(factor(ohareDest$ARR_HOUR, levels = 0:23))))
ohareJuneDepHourlyData <- na.omit(as.data.frame(table(factor(ohareOrigin$DEP_HOUR, levels = 0:23))))

midwayJuneHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
midwayJuneHourlyData$Arr <- midwayJuneArrHourlyData$Freq
midwayJuneHourlyData$Dep <- midwayJuneDepHourlyData$Freq

ohareJuneHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
ohareJuneHourlyData$Arr <- ohareJuneArrHourlyData$Freq
ohareJuneHourlyData$Dep <- ohareJuneDepHourlyData$Freq

#july
midwayJulyArrHourlyData <- na.omit(as.data.frame(table(factor(midwayDest$ARR_HOUR, levels = 0:23))))
midwayJulyDepHourlyData <- na.omit(as.data.frame(table(factor(midwayOrigin$DEP_HOUR, levels = 0:23))))

ohareJulyArrHourlyData <- na.omit(as.data.frame(table(factor(ohareDest$ARR_HOUR, levels = 0:23))))
ohareJulyDepHourlyData <- na.omit(as.data.frame(table(factor(ohareOrigin$DEP_HOUR, levels = 0:23))))

midwayJulyHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
midwayJulyHourlyData$Arr <- midwayJulyArrHourlyData$Freq
midwayJulyHourlyData$Dep <- midwayJulyDepHourlyData$Freq

ohareJulyHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
ohareJulyHourlyData$Arr <- ohareJulyArrHourlyData$Freq
ohareJulyHourlyData$Dep <- ohareJulyDepHourlyData$Freq

#August
midwayAugArrHourlyData <- na.omit(as.data.frame(table(factor(midwayDest$ARR_HOUR, levels = 0:23))))
midwayAugDepHourlyData <- na.omit(as.data.frame(table(factor(midwayOrigin$DEP_HOUR, levels = 0:23))))

ohareAugArrHourlyData <- na.omit(as.data.frame(table(factor(ohareDest$ARR_HOUR, levels = 0:23))))
ohareAugDepHourlyData <- na.omit(as.data.frame(table(factor(ohareOrigin$DEP_HOUR, levels = 0:23))))

midwayAugHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
midwayAugHourlyData$Arr <- midwayAugArrHourlyData$Freq
midwayAugHourlyData$Dep <- midwayAugDepHourlyData$Freq

ohareAugHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
ohareAugHourlyData$Arr <- ohareAugArrHourlyData$Freq
ohareAugHourlyData$Dep <- ohareAugDepHourlyData$Freq

#september
midwaySepArrHourlyData <- na.omit(as.data.frame(table(factor(midwayDest$ARR_HOUR, levels = 0:23))))
midwaySepDepHourlyData <- na.omit(as.data.frame(table(factor(midwayOrigin$DEP_HOUR, levels = 0:23))))

ohareSepArrHourlyData <- na.omit(as.data.frame(table(factor(ohareDest$ARR_HOUR, levels = 0:23))))
ohareSepDepHourlyData <- na.omit(as.data.frame(table(factor(ohareOrigin$DEP_HOUR, levels = 0:23))))

midwaySepHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
midwaySepHourlyData$Arr <- midwaySepArrHourlyData$Freq
midwaySepHourlyData$Dep <- midwaySepDepHourlyData$Freq

ohareSepHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
ohareSepHourlyData$Arr <- ohareSepArrHourlyData$Freq
ohareSepHourlyData$Dep <- ohareSepDepHourlyData$Freq

#october
midwayOctArrHourlyData <- na.omit(as.data.frame(table(factor(midwayDest$ARR_HOUR, levels = 0:23))))
midwayOctDepHourlyData <- na.omit(as.data.frame(table(factor(midwayOrigin$DEP_HOUR, levels = 0:23))))

ohareOctArrHourlyData <- na.omit(as.data.frame(table(factor(ohareDest$ARR_HOUR, levels = 0:23))))
ohareOctDepHourlyData <- na.omit(as.data.frame(table(factor(ohareOrigin$DEP_HOUR, levels = 0:23))))

midwayOctHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
midwayOctHourlyData$Arr <- midwayOctArrHourlyData$Freq
midwayOctHourlyData$Dep <- midwayOctDepHourlyData$Freq

ohareOctHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
ohareOctHourlyData$Arr <- ohareOctArrHourlyData$Freq
ohareOctHourlyData$Dep <- ohareOctDepHourlyData$Freq

#november
midwayNovArrHourlyData <- na.omit(as.data.frame(table(factor(midwayDest$ARR_HOUR, levels = 0:23))))
midwayNovDepHourlyData <- na.omit(as.data.frame(table(factor(midwayOrigin$DEP_HOUR, levels = 0:23))))

ohareNovArrHourlyData <- na.omit(as.data.frame(table(factor(ohareDest$ARR_HOUR, levels = 0:23))))
ohareNovDepHourlyData <- na.omit(as.data.frame(table(factor(ohareOrigin$DEP_HOUR, levels = 0:23))))

midwayNovHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
midwayNovHourlyData$Arr <- midwayNovArrHourlyData$Freq
midwayNovHourlyData$Dep <- midwayNovDepHourlyData$Freq

ohareNovHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
ohareNovHourlyData$Arr <- ohareNovArrHourlyData$Freq
ohareNovHourlyData$Dep <- ohareNovDepHourlyData$Freq

#december
midwayDecArrHourlyData <- na.omit(as.data.frame(table(factor(midwayDest$ARR_HOUR, levels = 0:23))))
midwayDecDepHourlyData <- na.omit(as.data.frame(table(factor(midwayOrigin$DEP_HOUR, levels = 0:23))))

ohareDecArrHourlyData <- na.omit(as.data.frame(table(factor(ohareDest$ARR_HOUR, levels = 0:23))))
ohareDecDepHourlyData <- na.omit(as.data.frame(table(factor(ohareOrigin$DEP_HOUR, levels = 0:23))))

midwayDecHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
midwayDecHourlyData$Arr <- midwayDecArrHourlyData$Freq
midwayDecHourlyData$Dep <- midwayDecDepHourlyData$Freq

ohareDecHourlyData <- data.frame("Hour" = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
ohareDecHourlyData$Arr <- ohareDecArrHourlyData$Freq
ohareDecHourlyData$Dep <- ohareDecDepHourlyData$Freq

#-----------------------------------------------------------------------------------------------------------------------------------

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

#Delay Sum Data
byMonthDelay <- c()

midJanDelayTemp <- subset(januaryFlightData, januaryFlightData$ORIGIN_AIRPORT_ID == "13232")
midJanDelay <- aggregate(cbind(midJanDelayTemp$CANCELLED)~midJanDelayTemp$DEP_HOUR, data=midJanDelayTemp, FUN=sum)
oJanDelayTemp <- subset(januaryFlightData, januaryFlightData$ORIGIN_AIRPORT_ID == "13930")
oJanDelay <- aggregate(cbind(oJanDelayTemp$CANCELLED)~oJanDelayTemp$DEP_HOUR, data=oJanDelayTemp, FUN=sum)
byMonthDelay <- rbind(byMonthDelay, t(aggregate(januaryFlightData$CANCELLED ~ januaryFlightData$CANCELLATION_CODE, januaryFlightData, sum)$'januaryFlightData$CANCELLED'))

midFebDelayTemp <- subset(februaryFlightData, februaryFlightData$ORIGIN_AIRPORT_ID == "13232")
midFebDelay <- aggregate(cbind(midFebDelayTemp$CANCELLED)~midFebDelayTemp$DEP_HOUR, data=midFebDelayTemp, FUN=sum)
oFebDelayTemp <- subset(februaryFlightData, februaryFlightData$ORIGIN_AIRPORT_ID == "13930")
oFebDelay <- aggregate(cbind(oFebDelayTemp$CANCELLED)~oFebDelayTemp$DEP_HOUR, data=oFebDelayTemp, FUN=sum)

midMarDelayTemp <- subset(marchFlightData, marchFlightData$ORIGIN_AIRPORT_ID == "13232")
midMarDelay <- aggregate(cbind(midMarDelayTemp$CANCELLED)~midMarDelayTemp$DEP_HOUR, data=midMarDelayTemp, FUN=sum)
oMarDelayTemp <- subset(marchFlightData, marchFlightData$ORIGIN_AIRPORT_ID == "13930")
oMarDelay <- aggregate(cbind(oMarDelayTemp$CANCELLED)~oMarDelayTemp$DEP_HOUR, data=oMarDelayTemp, FUN=sum)

midAprDelayTemp <- subset(aprilFlightData, aprilFlightData$ORIGIN_AIRPORT_ID == "13232")
midAprDelay <- aggregate(cbind(midAprDelayTemp$CANCELLED)~midAprDelayTemp$DEP_HOUR, data=midAprDelayTemp, FUN=sum)
oAprDelayTemp <- subset(aprilFlightData, aprilFlightData$ORIGIN_AIRPORT_ID == "13930")
oAprDelay <- aggregate(cbind(oAprDelayTemp$CANCELLED)~oAprDelayTemp$DEP_HOUR, data=oAprDelayTemp, FUN=sum)

midMayDelayTemp <- subset(mayFlightData, mayFlightData$ORIGIN_AIRPORT_ID == "13232")
midMayDelay <- aggregate(cbind(midMayDelayTemp$CANCELLED)~midMayDelayTemp$DEP_HOUR, data=midMayDelayTemp, FUN=sum)
oMayDelayTemp <- subset(mayFlightData, mayFlightData$ORIGIN_AIRPORT_ID == "13930")
oMayDelay <- aggregate(cbind(oMayDelayTemp$CANCELLED)~oMayDelayTemp$DEP_HOUR, data=oMayDelayTemp, FUN=sum)

midJunDelayTemp <- subset(juneFlightData, juneFlightData$ORIGIN_AIRPORT_ID == "13232")
midJunDelay <- aggregate(cbind(midJunDelayTemp$CANCELLED)~midJunDelayTemp$DEP_HOUR, data=midJunDelayTemp, FUN=sum)
oJunDelayTemp <- subset(juneFlightData, juneFlightData$ORIGIN_AIRPORT_ID == "13930")
oJunDelay <- aggregate(cbind(oJunDelayTemp$CANCELLED)~oJunDelayTemp$DEP_HOUR, data=oJunDelayTemp, FUN=sum)

midJulDelayTemp <- subset(julyFlightData, julyFlightData$ORIGIN_AIRPORT_ID == "13232")
midJulDelay <- aggregate(cbind(midJulDelayTemp$CANCELLED)~midJulDelayTemp$DEP_HOUR, data=midJulDelayTemp, FUN=sum)
oJulDelayTemp <- subset(julyFlightData, julyFlightData$ORIGIN_AIRPORT_ID == "13930")
oJulDelay <- aggregate(cbind(oJulDelayTemp$CANCELLED)~oJulDelayTemp$DEP_HOUR, data=oJulDelayTemp, FUN=sum)

midAugDelayTemp <- subset(augustFlightData, augustFlightData$ORIGIN_AIRPORT_ID == "13232")
midAugDelay <- aggregate(cbind(midAugDelayTemp$CANCELLED)~midAugDelayTemp$DEP_HOUR, data=midAugDelayTemp, FUN=sum)
oAugDelayTemp <- subset(augustFlightData, augustFlightData$ORIGIN_AIRPORT_ID == "13930")
oAugDelay <- aggregate(cbind(oAugDelayTemp$CANCELLED)~oAugDelayTemp$DEP_HOUR, data=oAugDelayTemp, FUN=sum)

midSepDelayTemp <- subset(septemberFlightData, septemberFlightData$ORIGIN_AIRPORT_ID == "13232")
midSepDelay <- aggregate(cbind(midSepDelayTemp$CANCELLED)~midSepDelayTemp$DEP_HOUR, data=midSepDelayTemp, FUN=sum)
oSepDelayTemp <- subset(septemberFlightData, septemberFlightData$ORIGIN_AIRPORT_ID == "13930")
oSepDelay <- aggregate(cbind(oSepDelayTemp$CANCELLED)~oSepDelayTemp$DEP_HOUR, data=oSepDelayTemp, FUN=sum)

midOctDelayTemp <- subset(octoberFlightData, octoberFlightData$ORIGIN_AIRPORT_ID == "13232")
midOctDelay <- aggregate(cbind(midOctDelayTemp$CANCELLED)~midOctDelayTemp$DEP_HOUR, data=midOctDelayTemp, FUN=sum)
oOctDelayTemp <- subset(octoberFlightData, octoberFlightData$ORIGIN_AIRPORT_ID == "13930")
oOctDelay <- aggregate(cbind(oOctDelayTemp$CANCELLED)~oOctDelayTemp$DEP_HOUR, data=oOctDelayTemp, FUN=sum)

midNovDelayTemp <- subset(novemberFlightData, novemberFlightData$ORIGIN_AIRPORT_ID == "13232")
midNovDelay <- aggregate(cbind(midNovDelayTemp$CANCELLED)~midNovDelayTemp$DEP_HOUR, data=midNovDelayTemp, FUN=sum)
oNovDelayTemp <- subset(novemberFlightData, novemberFlightData$ORIGIN_AIRPORT_ID == "13930")
oNovDelay <- aggregate(cbind(oNovDelayTemp$CANCELLED)~oNovDelayTemp$DEP_HOUR, data=oNovDelayTemp, FUN=sum)

midDecDelayTemp <- subset(decemberFlightData, decemberFlightData$ORIGIN_AIRPORT_ID == "13232")
midDecDelay <- aggregate(cbind(midDecDelayTemp$CANCELLED)~midDecDelayTemp$DEP_HOUR, data=midDecDelayTemp, FUN=sum)
oDecDelayTemp <- subset(decemberFlightData, decemberFlightData$ORIGIN_AIRPORT_ID == "13930")
oDecDelay <- aggregate(cbind(oDecDelayTemp$CANCELLED)~oDecDelayTemp$DEP_HOUR, data=oDecDelayTemp, FUN=sum)

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
      menuItem("About", icon = icon("info"), href = "http://cjanow3.people.uic.edu/project2.html")
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
                box(title = "O'Hare Airline Hourly Arrival & Departure as Line Chart", solidHeader = TRUE, status = "primary", width = 12, plotOutput("ohareJanHourlyLine"))
              
              ),
              fluidRow(
                box(title = "O'Hare Airline Hourly Arrival & Departure Table", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("ohareJanHourlyTable"))
              ),
              fluidRow(
                box(title = "O'Hare Day Airline Data as Table", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("ohareDayDataTable")
                )
              ),
              fluidRow(
                box(title = "O'Hare Delays", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("oJanDelayTable"))
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
                box(title = "Midway Airline Hourly Arrival & Departure Table", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("midwayJanHourlyTable"))
              ),
              fluidRow(
                box(title = "Midway Airline Hourly Arrival & Departure as Line Chart", solidHeader = TRUE, status = "primary", width = 12, plotOutput("midwayJanHourlyLine"))
              ),
              fluidRow(
                box(title = "Midway Day Airline Data as Table", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("midwayDayDataTable")
                )
              ),
              fluidRow(
                box(title = "Midway Delays", solidHeader = TRUE, status = "primary", width = 12, dataTableOutput("midJanDelayTable"))
              )
      )
    )
  )
)

server <- function(input, output) { 
  
  # Tables for all domestic airlines (bullet point 1)
  
  output$ohareAirlinesTable <- DT::renderDataTable(ohareAirlines, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE))
  output$midwayAirlinesTable <- DT::renderDataTable(midwayAirlines, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE))
  output$midJanDelayTable <- DT::renderDataTable(midJanDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$oJanDelayTable <- DT::renderDataTable(oJanDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$midFebDelayTable <- DT::renderDataTable(midFebDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$oFebDelayTable <- DT::renderDataTable(oFebDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$midMarDelayTable <- DT::renderDataTable(midMarDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$oMarDelayTable <- DT::renderDataTable(oMarDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$midAprDelayTable <- DT::renderDataTable(midAprDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$oAprDelayTable <- DT::renderDataTable(oAprDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$midMayDelayTable <- DT::renderDataTable(midMayDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$oMayDelayTable <- DT::renderDataTable(oMayDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$midJunDelayTable <- DT::renderDataTable(midJunDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$oJunDelayTable <- DT::renderDataTable(oJunDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$midJulDelayTable <- DT::renderDataTable(midJulDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$oJulDelayTable <- DT::renderDataTable(oJulDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$midAugDelayTable <- DT::renderDataTable(midAugDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$oAugDelayTable <- DT::renderDataTable(oAugDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$midSepDelayTable <- DT::renderDataTable(midSepDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$oSepDelayTable <- DT::renderDataTable(oSepDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$midOctDelayTable <- DT::renderDataTable(midOctDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$oOctDelayTable <- DT::renderDataTable(oOctDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$midNovDelayTable <- DT::renderDataTable(midNovDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$oNovDelayTable <- DT::renderDataTable(oNovDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$midDecDelayTable <- DT::renderDataTable(midDecDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  output$oDecDelayTable <- DT::renderDataTable(oDecDelay, server = TRUE, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE), colnames = c('Flight Hour','Delays'))
  
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
  
  #Hourly arrival Line charts
  output$midwayJanHourlyLine <- renderPlot({
    ggplot(midwayJanHourlyData, aes(x=midwayJanHourlyData$Hour)) + geom_line(aes(y=midwayJanHourlyData$Arr, color = "Arrivals")) +
    geom_line(aes(y=midwayJanHourlyData$Dep, color = "Departures")) +  labs(x="Hour", y="Number of Flights") +
    scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$ohareJanHourlyLine <- renderPlot({
    ggplot(ohareJanHourlyData, aes(x=ohareJanHourlyData$Hour)) + geom_line(aes(y=ohareJanHourlyData$Arr, color = "Arrivals")) +
    geom_line(aes(y=ohareJanHourlyData$Dep, color = "Departures")) + labs(x="Hour", y="Number of Flights") +
    scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$midwayFebHourlyLine <- renderPlot({
    ggplot(midwayFebHourlyData, aes(x=midwayFebHourlyData$Hour)) + geom_line(aes(y=midwayFebHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=midwayFebHourlyData$Dep, color = "Departures")) +  labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$ohareFebHourlyLine <- renderPlot({
    ggplot(ohareFebHourlyData, aes(x=ohareFebHourlyData$Hour)) + geom_line(aes(y=ohareFebHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=ohareFebHourlyData$Dep, color = "Departures")) + labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$midwayMarchHourlyLine <- renderPlot({
    ggplot(midwayMarchHourlyData, aes(x=midwayMarchHourlyData$Hour)) + geom_line(aes(y=midwayMarchHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=midwayMarchHourlyData$Dep, color = "Departures")) +  labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$ohareMarchHourlyLine <- renderPlot({
    ggplot(ohareMarchHourlyData, aes(x=ohareMarchHourlyData$Hour)) + geom_line(aes(y=ohareMarchHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=ohareMarchHourlyData$Dep, color = "Departures")) + labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$midwayAprilHourlyLine <- renderPlot({
    ggplot(midwayAprilHourlyData, aes(x=midwayAprilHourlyData$Hour)) + geom_line(aes(y=midwayAprilHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=midwayAprilHourlyData$Dep, color = "Departures")) +  labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$ohareAprilHourlyLine <- renderPlot({
    ggplot(ohareAprilHourlyData, aes(x=ohareAprilHourlyData$Hour)) + geom_line(aes(y=ohareAprilHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=ohareAprilHourlyData$Dep, color = "Departures")) + labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$midwayMayHourlyLine <- renderPlot({
    ggplot(midwayMayHourlyData, aes(x=midwayMayHourlyData$Hour)) + geom_line(aes(y=midwayMayHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=midwayMayHourlyData$Dep, color = "Departures")) +  labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$ohareMayHourlyLine <- renderPlot({
    ggplot(ohareMayHourlyData, aes(x=ohareMayHourlyData$Hour)) + geom_line(aes(y=ohareMayHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=ohareMayHourlyData$Dep, color = "Departures")) + labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$midwayJuneHourlyLine <- renderPlot({
    ggplot(midwayJuneHourlyData, aes(x=midwayJuneHourlyData$Hour)) + geom_line(aes(y=midwayJuneHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=midwayJuneHourlyData$Dep, color = "Departures")) +  labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$ohareJuneHourlyLine <- renderPlot({
    ggplot(ohareJuneHourlyData, aes(x=ohareJuneHourlyData$Hour)) + geom_line(aes(y=ohareJuneHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=ohareJuneHourlyData$Dep, color = "Departures")) + labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$midwayJulyHourlyLine <- renderPlot({
    ggplot(midwayJulyHourlyData, aes(x=midwayJulyHourlyData$Hour)) + geom_line(aes(y=midwayJulyHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=midwayJulyHourlyData$Dep, color = "Departures")) +  labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$ohareJulyHourlyLine <- renderPlot({
    ggplot(ohareJulyHourlyData, aes(x=ohareJulyHourlyData$Hour)) + geom_line(aes(y=ohareJulyHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=ohareJulyHourlyData$Dep, color = "Departures")) + labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$midwayAugHourlyLine <- renderPlot({
    ggplot(midwayAugHourlyData, aes(x=midwayAugHourlyData$Hour)) + geom_line(aes(y=midwayAugHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=midwayAugHourlyData$Dep, color = "Departures")) +  labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$ohareAugHourlyLine <- renderPlot({
    ggplot(ohareAugHourlyData, aes(x=ohareAugHourlyData$Hour)) + geom_line(aes(y=ohareAugHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=ohareAugHourlyData$Dep, color = "Departures")) + labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$midwaySepHourlyLine <- renderPlot({
    ggplot(midwaySepHourlyData, aes(x=midwaySepHourlyData$Hour)) + geom_line(aes(y=midwaySepHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=midwaySepHourlyData$Dep, color = "Departures")) +  labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$ohareSepHourlyLine <- renderPlot({
    ggplot(ohareSepHourlyData, aes(x=ohareSepHourlyData$Hour)) + geom_line(aes(y=ohareSepHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=ohareSepHourlyData$Dep, color = "Departures")) + labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })  
  output$midwayOctHourlyLine <- renderPlot({
    ggplot(midwayOctHourlyData, aes(x=midwayOctHourlyData$Hour)) + geom_line(aes(y=midwayOctHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=midwayOctHourlyData$Dep, color = "Departures")) +  labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$ohareOctHourlyLine <- renderPlot({
    ggplot(ohareOctHourlyData, aes(x=ohareOctHourlyData$Hour)) + geom_line(aes(y=ohareOctHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=ohareOctHourlyData$Dep, color = "Departures")) + labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })  
  output$midwayNovHourlyLine <- renderPlot({
    ggplot(midwayNovHourlyData, aes(x=midwayNovHourlyData$Hour)) + geom_line(aes(y=midwayNovHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=midwayNovHourlyData$Dep, color = "Departures")) +  labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$ohareNovHourlyLine <- renderPlot({
    ggplot(ohareNovHourlyData, aes(x=ohareNovHourlyData$Hour)) + geom_line(aes(y=ohareNovHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=ohareNovHourlyData$Dep, color = "Departures")) + labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })  
  output$midwayDecHourlyLine <- renderPlot({
    ggplot(midwayDecHourlyData, aes(x=midwayDecHourlyData$Hour)) + geom_line(aes(y=midwayDecHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=midwayDecHourlyData$Dep, color = "Departures")) +  labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })
  output$ohareDecHourlyLine <- renderPlot({
    ggplot(ohareDecHourlyData, aes(x=ohareDecHourlyData$Hour)) + geom_line(aes(y=ohareDecHourlyData$Arr, color = "Arrivals")) +
      geom_line(aes(y=ohareDecHourlyData$Dep, color = "Departures")) + labs(x="Hour", y="Number of Flights") +
      scale_color_manual(name = "Legend", values = c("blue1", "red2"))  
  })   
  
  #Tables for hourly data
  output$ohareJanHourlyTable <- DT::renderDataTable(ohareJanHourlyData, server = TRUE, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
  output$midwayJanHourlyTable <- DT::renderDataTable(midwayJanHourlyData, server = TRUE, options = list(pageLength = 8, lengthChange = FALSE, seraching = FALSE))
  output$ohareFebHourlyTable <- DT::renderDataTable(ohareFebHourlyData, server = TRUE, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
  output$midwayFebHourlyTable <- DT::renderDataTable(midwayFebHourlyData, server = TRUE, options = list(pageLength = 8, lengthChange = FALSE, seraching = FALSE))
  output$ohareMarchHourlyTable <- DT::renderDataTable(ohareMarchHourlyData, server = TRUE, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
  output$midwayMarchHourlyTable <- DT::renderDataTable(midwayMarchHourlyData, server = TRUE, options = list(pageLength = 8, lengthChange = FALSE, seraching = FALSE))
  output$ohareAprilHourlyTable <- DT::renderDataTable(ohareAprilHourlyData, server = TRUE, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
  output$midwayAprilHourlyTable <- DT::renderDataTable(midwayAprilHourlyData, server = TRUE, options = list(pageLength = 8, lengthChange = FALSE, seraching = FALSE))
  output$ohareMayHourlyTable <- DT::renderDataTable(ohareMayHourlyData, server = TRUE, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
  output$midwayMayHourlyTable <- DT::renderDataTable(midwayMayHourlyData, server = TRUE, options = list(pageLength = 8, lengthChange = FALSE, seraching = FALSE))
  output$ohareJuneHourlyTable <- DT::renderDataTable(ohareJuneHourlyData, server = TRUE, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
  output$midwayJuneHourlyTable <- DT::renderDataTable(midwayJuneHourlyData, server = TRUE, options = list(pageLength = 8, lengthChange = FALSE, seraching = FALSE))
  output$ohareJulyHourlyTable <- DT::renderDataTable(ohareJulyHourlyData, server = TRUE, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
  output$midwayJulyHourlyTable <- DT::renderDataTable(midwayJulyHourlyData, server = TRUE, options = list(pageLength = 8, lengthChange = FALSE, seraching = FALSE))
  output$ohareAugHourlyTable <- DT::renderDataTable(ohareAugHourlyData, server = TRUE, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
  output$midwayAugHourlyTable <- DT::renderDataTable(midwayAugHourlyData, server = TRUE, options = list(pageLength = 8, lengthChange = FALSE, seraching = FALSE))
  output$ohareSepHourlyTable <- DT::renderDataTable(ohareSepHourlyData, server = TRUE, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
  output$midwaySepHourlyTable <- DT::renderDataTable(midwaySepHourlyData, server = TRUE, options = list(pageLength = 8, lengthChange = FALSE, seraching = FALSE))
  output$ohareOctHourlyTable <- DT::renderDataTable(ohareOctHourlyData, server = TRUE, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
  output$midwayOctHourlyTable <- DT::renderDataTable(midwayOctHourlyData, server = TRUE, options = list(pageLength = 8, lengthChange = FALSE, seraching = FALSE))
  output$ohareNovHourlyTable <- DT::renderDataTable(ohareNovHourlyData, server = TRUE, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
  output$midwayNovHourlyTable <- DT::renderDataTable(midwayNovHourlyData, server = TRUE, options = list(pageLength = 8, lengthChange = FALSE, seraching = FALSE))
  output$ohareDecHourlyTable <- DT::renderDataTable(ohareDecHourlyData, server = TRUE, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
  output$midwayDecHourlyTable <- DT::renderDataTable(midwayDecHourlyData, server = TRUE, options = list(pageLength = 8, lengthChange = FALSE, seraching = FALSE))
  
  # Tables for each day of the week for ohare and midway
  output$ohareDayDataTable <- DT::renderDataTable(ohareDayData, server = TRUE, options = list(pageLength = 7, lengthChange = FALSE, searching = FALSE))
  output$midwayDayDataTable <- DT::renderDataTable(midwayDayData, server = TRUE, options = list(pageLength = 7, lengthChange = FALSE, searching = FALSE))
  
}

shinyApp(ui, server)