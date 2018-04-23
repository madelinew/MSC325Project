#Importing Data
library(readxl)
Data <- read_excel("~/Documents/MSC325/Data.xlsx")

#Separating Year and Month from Time
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library(tidyr)

#Function for Splitting Time column into Month and Year
splitTime <- function(df) {
  #df = Data
  ndf <- df
  ndf <- transform(ndf, Year = substr(Time, 1, 4), Month = substr(Time, 5,6), stringsAsFactors = FALSE)
  ndf$Year <- as.integer(ndf$Year)
  ndf$Month <- as.integer(ndf$Month)
  return(ndf)
}

airData <- splitTime(Data)

#Function for reformatting airData to have Year, Month, fullData, MonthName, ArrivalOnTime
reformatDF <- function(df){
  #df = airData
  ndf <- df
  ndf$fullDate <- with(ndf, sprintf("%02d/%d",Month, Year))
  ndf$MonthName <- month.name[ndf$Month]
  ndf$ArrivalsOnTime <- ndf$Airline.Arrivals.On.Time.....USA.....
  drop <- c("Time","Airline.Arrivals.On.Time.....USA.....")
  ndf = ndf[,!(names(ndf) %in% drop)]
  return(ndf)
}
realdf <- reformatDF(airData)
write.csv(realdf, file="AirlineData.csv")

summary(realdf)


