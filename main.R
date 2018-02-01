rm(list=ls())

source("E:\\Michael\\Comsol\\HomelessProject\\functions.R")

loadLibraries()
 
setwd("E:\\Michael\\Comsol\\LatestDataCSVs")

#---create variables for file names----------
homelessDataRawFile <- "homelessdata.csv"
homelessDataLatestNumbers <- "homeslessApplicationNumbers.csv"
housingAdviceCasesWithOutcomes <- "HA Cases closed between Apr 2012 Dec 2017.csv"
housingAdviceReasons <- "housingAdviceCases2.csv"
temporaryAccomodationData <- "TA Data April 2013 to present with UPRN.csv"

#---create dataframes for data---------------
homelessDataRawDF <- read.csv(homelessDataRawFile, stringsAsFactors = FALSE) %>% unique()
homelessDataLatestNumbersDF <- read.csv(homelessDataLatestNumbers, stringsAsFactors = FALSE) %>% unique()
housingAdviceCasesWithOutcomesDF <- read.csv(housingAdviceCasesWithOutcomes, stringsAsFactors = FALSE) %>% unique()
housingAdviceReasonsDF <- read.csv(housingAdviceReasons, stringsAsFactors = FALSE) %>% unique()
temporaryAccomodationDataDF <- read.csv(temporaryAccomodationData, stringsAsFactors = FALSE) %>% unique()

#---Change dates to dates-------------------
homelessDataRawDF$Application.Date <- dmy(homelessDataRawDF$Application.Date)
homelessDataLatestNumbersDF$Date.Logged <- dmy(homelessDataLatestNumbersDF$Date.Logged)
housingAdviceCasesWithOutcomesDF$Date.Logged <- dmy(housingAdviceCasesWithOutcomesDF$Date.Logged)
housingAdviceCasesWithOutcomesDF$Date.Closed <- dmy(housingAdviceCasesWithOutcomesDF$Date.Closed)
housingAdviceReasonsDF$actiondate <- dmy(housingAdviceReasonsDF$actiondate)
temporaryAccomodationDataDF$Tenancy_Start_Date <- dmy(temporaryAccomodationDataDF$Tenancy_Start_Date)
temporaryAccomodationDataDF$Tenancy_End_Date <- dmy(temporaryAccomodationDataDF$Tenancy_End_Date)

#---clip data to April 2012 onwards---------
homelessDataRawDF <- homelessDataRawDF[homelessDataRawDF$Application.Date >= dmy("01042012"),]
homelessDataLatestNumbersDF <- homelessDataLatestNumbersDF[homelessDataLatestNumbersDF$Date.Logged >= dmy('01042012'),]
housingAdviceReasonsDF <- housingAdviceReasonsDF[housingAdviceReasonsDF$actiondate >= dmy('01042012'),]

#remove white spaces in stop reasons and TENANCY ERRORS
trimws(temporaryAccomodationDataDF$Tenancy_Stop_Reason)
temporaryAccomodationDataDF <- temporaryAccomodationDataDF[temporaryAccomodationDataDF$Tenancy_Stop_Reason != 'TENANCY ERROR',]

#---create hcode in homeless application data-------
homelessDataRawDF$hcode <- paste0(homelessDataRawDF$Homelessness.Ref, '/', homelessDataRawDF$Homelessness.Suffix)
homelessDataLatestNumbersDF$hcode <- paste0(homelessDataLatestNumbersDF$Homeless.Ref, '/', homelessDataLatestNumbersDF$Suffix)

#---create dataframe to hold comparible data--------
monthlyValues <- createFrameWork()


#---create period value in relevant Dataframes-------------------
homelessDataLatestNumbersDF$period <- period(homelessDataLatestNumbersDF$Date.Logged)
housingAdviceReasonsDF$period <- period(housingAdviceReasonsDF$actiondate)
temporaryAccomodationDataDF$period <- period(temporaryAccomodationDataDF$Tenancy_Start_Date)


#---remove Welfare Reform Team data from housing advice---------
housingAdviceCasesWithOutcomesDF <- merge(housingAdviceReasonsDF, 
                                          housingAdviceCasesWithOutcomesDF, by.y = "Case.Number", by.x = "caseno", all.x = TRUE)

#housingAdviceCasesWithOutcomesDF <- 
argument <- housingAdviceReasonsDF$enquiry.type != "WELFARE REFORM" | is.na(housingAdviceReasonsDF$enquiry.type)
housingAdviceReasonsDF <- housingAdviceReasonsDF[argument,]
housingAdviceReasonsDF <- unique(housingAdviceReasonsDF)

remove(argument)

#homeless data
monthlyValues <- mergeData(monthlyValues, homelessDataLatestNumbersDF, "homelessData")

#housing advice
monthlyValues <- mergeData(monthlyValues, housingAdviceReasonsDF, "housingAdvice")

#temporary accomodation data

monthlyValues <- mergeData(monthlyValues, unique(temporaryAccomodationDataDF[c(1,9,10, 44)]), "temporaryAccomodation")


#---examine the results-------------------------
ggValues <- gather(monthlyValues, "type", "count", 2:4)

ggplot(ggValues, aes(x=factor(period), y=count, colour=type, group=type)) + geom_line() + geom_smooth() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8))

#--Timeseries Analysis----------------------------

#use the following function to create time series stl
#   timeSeries(x, starty, startm, endy, endm, name)

homelessTS <- newTimeSeries(monthlyValues[c(1,2)], "homelessness")
plot(homelessTS, main="Homeless Applications Time Series Decomposition")
dev.copy(jpeg,"E:\\Michael\\Comsol\\Charts\\Homeless Applications Time series Analysis.jpg", width=1800,height=1200)
dev.off()

housingAdviceTS <- newTimeSeries(monthlyValues[c(1,3)], "housingAdvice")
plot(housingAdviceTS, main="Housing Advice Time Series Decomposition")
dev.copy(jpeg,"E:\\Michael\\Comsol\\Charts\\Housing Advice Time series Analysis.jpg", width=1800,height=1200)
dev.off()

temporaryAccomodationTS <- newTimeSeries(monthlyValues[c(1,4)], "temporaryAccomodation")
plot(temporaryAccomodationTS, main="Temporary Accomodation Time Series Decomposition")
dev.copy(jpeg,"E:\\Michael\\Comsol\\Charts\\Temporary Accomodation Time series Analysis.jpg", width=1800,height=1200)
dev.off()


#--forecasting-------------------------------------

#Homelessness
homelessForecasting <- carryOutTimeSeries(monthlyValues[c(1,2)])
homelessPlot <- ggPlotForecast(homelessForecasting, "Homeless Applications")
homelessPlot

#Housing Advice
housingAdviceForecasting <- carryOutTimeSeries(monthlyValues[c(1,3)])
housingAdvicePlot <- ggPlotForecast(housingAdviceForecasting, "Housing Advice")
housingAdvicePlot

#Temporary Accomodation
temporaryAccomodationForecasting <- carryOutTimeSeries(monthlyValues[c(1,4)])
temporaryAccomodationPlot <- ggPlotForecast(temporaryAccomodationForecasting, "Temporary Accomodation")
temporaryAccomodationPlot

#checking predictions against real values
tail(homelessForecasting,20)[1:10,]
tail(housingAdviceForecasting,20)[1:10,]
tail(temporaryAccomodationForecasting,20)[1:10,]



