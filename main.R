rm(list=ls())

source("E:\\Michael\\Comsol\\HomelessProject\\functions.R")

loadLibraries()
 
setwd("E:\\Michael\\Comsol\\LatestDataCSVs")

#---create variables for file names----------
homelessDataRawFile <- "homelessdata.csv"
homelessDataLatestNumbers <- "homeslessApplicationNumbers.csv"
housingAdviceCasesWithOutcomes <- "\\\\Dsfin\\corpcom\\Michael_Sinclair\\HomelessData\\HA Cases 010412 to date.csv"
temporaryAccomodationData <- "TA Data April 2013 to present with UPRN.csv"

#---create dataframes for data---------------
homelessDataRawDF <- read.csv(homelessDataRawFile, stringsAsFactors = FALSE) %>% unique()
homelessDataLatestNumbersDF <- read.csv(homelessDataLatestNumbers, stringsAsFactors = FALSE) %>% unique()
housingAdviceCasesWithOutcomesDF <- read.csv(housingAdviceCasesWithOutcomes, stringsAsFactors = FALSE) %>% unique()
temporaryAccomodationDataDF <- read.csv(temporaryAccomodationData, stringsAsFactors = FALSE) %>% unique()

#---Change dates to dates-------------------
homelessDataRawDF$Application.Date <- dmy(homelessDataRawDF$Application.Date)
homelessDataLatestNumbersDF$Date.Logged <- dmy(homelessDataLatestNumbersDF$Date.Logged)
housingAdviceCasesWithOutcomesDF$Actualdate <- dmy(housingAdviceCasesWithOutcomesDF$Actualdate)
housingAdviceCasesWithOutcomesDF$Entrydate <- dmy(housingAdviceCasesWithOutcomesDF$Entrydate)
housingAdviceCasesWithOutcomesDF$Closedate <- dmy(housingAdviceCasesWithOutcomesDF$Closedate)
temporaryAccomodationDataDF$Tenancy_Start_Date <- dmy(temporaryAccomodationDataDF$Tenancy_Start_Date)
temporaryAccomodationDataDF$Tenancy_End_Date <- dmy(temporaryAccomodationDataDF$Tenancy_End_Date)
#homeless data quarterly breakdown
homelessDataLatestNumbersDF$quarter <- as.yearqtr(homelessDataLatestNumbersDF$Date.Logged, format = "%Y-%m-%d")
homelessDataRawDF$quarter <- as.yearqtr(homelessDataRawDF$Application.Date, format = "%Y-%m-%d")

#---clip data to April 2012 onwards---------
homelessDataRawDF <- homelessDataRawDF[homelessDataRawDF$Application.Date >= dmy("01042012"),]
homelessDataLatestNumbersDF <- homelessDataLatestNumbersDF[homelessDataLatestNumbersDF$Date.Logged >= dmy('01042012'),]
housingAdviceCasesWithOutcomesDF <- housingAdviceCasesWithOutcomesDF[housingAdviceCasesWithOutcomesDF$Actualdate >= dmy('01042012'),]
housingAdviceCasesWithOutcomesDF <- housingAdviceCasesWithOutcomesDF[housingAdviceCasesWithOutcomesDF$Actualdate < dmy('01032018'),]


#remove white spaces in stop reasons and TENANCY ERRORS
#trimws(temporaryAccomodationDataDF$Tenancy_Stop_Reason)
temporaryAccomodationDataDF <- temporaryAccomodationDataDF[temporaryAccomodationDataDF$Tenancy_Stop_Reason != 'TENANCY ERROR',]

#---create hcode in homeless application data-------
homelessDataRawDF$hcode <- paste0(homelessDataRawDF$Homelessness.Ref, '/', homelessDataRawDF$Homelessness.Suffix)
homelessDataLatestNumbersDF$hcode <- paste0(homelessDataLatestNumbersDF$Homeless.Ref, '/', homelessDataLatestNumbersDF$Suffix)

#---create dataframe to hold comparible data--------
monthlyValues <- createFrameWork()

#clean Housing advice reasons 
housingAdviceCasesWithOutcomesDF$Decode <- str_trim(housingAdviceCasesWithOutcomesDF$Decode, "both")
housingAdviceCasesWithOutcomesDF$Decode <- gsub("[.]", "", housingAdviceCasesWithOutcomesDF$Decode)

housingAdviceCasesWithOutcomesDF$Decode1 <- str_trim(housingAdviceCasesWithOutcomesDF$Decode1, "both")
housingAdviceCasesWithOutcomesDF$Decode1 <- gsub("[.]", "", housingAdviceCasesWithOutcomesDF$Decode1)


#---create period value in relevant Dataframes-------------------
homelessDataLatestNumbersDF$period <- period(homelessDataLatestNumbersDF$Date.Logged)
homelessDataRawDF$period <- period(homelessDataRawDF$Application.Date)
housingAdviceCasesWithOutcomesDF$period <- period(housingAdviceCasesWithOutcomesDF$Actualdate)
temporaryAccomodationDataDF$period <- period(temporaryAccomodationDataDF$Tenancy_Start_Date)


# #housingAdviceCasesWithOutcomesDF <- 
# argument <- housingAdviceCasesWithOutcomesDF$Decode != "WELFARE REFORM" | is.na(housingAdviceCasesWithOutcomesDF$Decode)
# housingAdviceReasonsDF <- housingAdviceReasonsDF[argument,]
# housingAdviceReasonsDF <- unique(housingAdviceReasonsDF)

#---remove Welfare Reform Team data from housing advice---------
#housingAdviceCasesWithOutcomesDF <- merge(housingAdviceReasonsDF, 
#                                          housingAdviceCasesWithOutcomesDF, by.y = "Case.Number", by.x = "caseno", all.x = TRUE)


#remove(argument)

#homeless data
monthlyValues <- mergeData(monthlyValues, homelessDataLatestNumbersDF, "homelessData")

#housing advice
monthlyValues <- mergeData(monthlyValues, housingAdviceCasesWithOutcomesDF, "housingAdvice")

#temporary accomodation data

monthlyValues <- mergeData(monthlyValues, unique(temporaryAccomodationDataDF[c(1,9,10, 44)]), "temporaryAccomodation")


#---examine the results-------------------------
ggValues <- gather(monthlyValues, "type", "count", 2:4)

figure1Plot <- ggplot(na.omit(ggValues), aes(x=factor(period), y=count, colour=type, group=type)) + geom_line() + geom_smooth() +
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
#homelessPlot

#Housing Advice
housingAdviceForecasting <- carryOutTimeSeries(monthlyValues[c(1,3)])
housingAdvicePlot <- ggPlotForecast(housingAdviceForecasting, "Housing Advice")
#housingAdvicePlot

#Temporary Accomodation
temporaryAccomodationForecasting <- carryOutTimeSeries(monthlyValues[c(1,4)])
temporaryAccomodationPlot <- ggPlotForecast(temporaryAccomodationForecasting, "Temporary Accomodation")
#temporaryAccomodationPlot





#------Housing Advice Reasons -----------------------------

housingAdviceCasesWithOutcomesDF %>% 
  filter(Actualdate >= dmy('010117'), Actualdate < dmy('010118')) %>% 
  group_by(Decode) %>% 
  summarise(count = n()) -> housingAdvice2017DF

housingAdvice2017DF

housingAdviceCasesWithOutcomesDF %>% 
  filter(Actualdate >= dmy('010116'), Actualdate < dmy('010117')) %>% 
  group_by(Decode) %>% 
  summarise(count = n()) -> housingAdvice2016DF

housingAdviceReasonCount <- merge(housingAdvice2016DF, housingAdvice2017DF, by="Decode")

housingAdviceReasonCount <- housingAdviceReasonCount[order(housingAdviceReasonCount$count.y),] %>% tail(10)

names(housingAdviceReasonCount) <- c("Reason", "2016", "2017")

housingAdviceReasonCount_gg <- gather(housingAdviceReasonCount, "Year", "count", 2:3)

housingAdviceReasonPlot <- ggplot(housingAdviceReasonCount_gg, aes(x=factor(Reason), y=count, fill = Year)) + geom_bar(stat='identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8)) +
  labs(title="Housing Advice Reasons: 2016 and 2017", x ="Month", y = paste0("Count"))




#------Rent Arrears worker -------------- story
housingAdviceCasesWithOutcomesDF %>% filter(Entryuserid == "FTHOMP", Actualdate >= dmy('010117')) %>% 
  group_by(period, Decode) %>% summarise(count=n()) -> explore
explore$Decode[explore$Decode != "RENT ARREARS ADVICE"] <- "Non-Rent Arrears"
explore$Decode[explore$Decode == "RENT ARREARS ADVICE"] <- "Rent Arrears"

#user FUNMI THOMPSON
ggRentArrearsPlot <- ggplot(explore, aes(x=factor(period), y=count, fill=Decode)) + geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8)) +
  labs(title="Number of Rent Arrears Cases per month undertaken by Rent Arrears Worker, 2017",
       x ="Month", y = paste0("Count"))



#closure reasons
ggRentArrearsClosureTable <- housingAdviceCasesWithOutcomesDF %>% 
  filter(Decode == "RENT ARREARS ADVICE", Entryuserid == "FTHOMP", Actualdate >= dmy('010117')) %>% 
  group_by(Decode1) %>% summarise(count=n())
ggRentArrearsClosureTable[is.na(ggRentArrearsClosureTable[1]),1] <- "Active Case"



