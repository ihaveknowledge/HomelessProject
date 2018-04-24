loadLibraries <- function(){
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(lubridate, ggplot2, tidyr, ggrepel, ggthemes, stringr, psych, forecast, zoo, stringr, rgdal, purrr, 
                 dplyr, infer, RColorBrewer, GISTools)
}




#createFrameWork to hold monthly values
createFrameWork <- function(){
a <- character()

for (i in 2012:2018)
{
  b <- str_pad(1:12, 2, pad = "0") %>%  paste0(i,.)
  a <- c(a,b)
}

monthlyValues <- data.frame(a[-1:-3], stringsAsFactors = F)
names(monthlyValues) <- "period"
remove(a,b,i)
return(monthlyValues)
}


#---create period function to extract yyyymm value from date for comparisson in above dataframe--------
period <- function(dateColumn) {
  period <- paste0(format(as.Date(dateColumn), "%Y"), format(as.Date(dateColumn), "%m"))
  return(period)
}

 

#---bring together basic numbers #homeless #housingadvice #ta---------
mergeData <- function(dataSetX, datasetY, dataSetName){
  temp <- table(datasetY$period) %>% as.data.frame()  
  names(temp) <- c("period", dataSetName)
  return(merge(dataSetX, temp, "period", all.x = TRUE))
  remove(temp)
}


#create time series function
timeSeries <- function(x, starty, startm, endy, endm, name)
{
  borough.ts <- x
  
  #---Create time series-----enter start date(year, mont)--end date(year, month)----frequency = 12 monthly, 52 weekly or 365 daily
  total.ts <- ts(borough.ts, start=c(starty, startm), end=c(endy, endm), frequency=12, names="applications")
  #---Create trend, seasonsal and remainder time series
  total.fit <- stl(total.ts, s.window="period")
  #--forecast---
  # forecasting using stl + random walk - rwdrift
  return(total.fit)
}

#create new time Series using period field, uses above time series method
newTimeSeries <- function(df, varname) {
  length <- df[is.na(df[2]) == FALSE,2] %>% length()  
  
  startY <- substr(min(df[[1]]), 1, 4) %>%  as.numeric()
  startM <- substr(min(df[[1]]), 5, 6) %>%  as.numeric()
  endY <- substr(df[length,1], 1, 4) %>%  as.numeric()
  endM  <- substr(df[length,1], 5, 6) %>%  as.numeric()
  name <- varname
  time <- timeSeries(df[1:length,2], startY, startM, endY, endM, name)
  return(time)
}

#forecasting for rolling period, using above time series method
forecasting <- function(total.fit) {
  borough.rw <- forecast(total.fit, method="rwdrift")
  #plot(borough.rw, main=name)
  
  new <- data.frame(borough.rw)[1]
  names(new) <- "data"
  new$date <- paste0('01 ',rownames(new)) %>% dmy()
  new$month <- format(as.Date(new$date), "%m")
  new$year <- format(as.Date(new$date), "%Y")
  new$monthyear <- paste0(new$year, new$month) %>% as.numeric()
  new <- new[c('monthyear', 'data')]
  return(new)
  
}

#create dataframe for Forecasting
createTimeSeriesDataFrame <- function(DFforTimeSeries) {
  #set up period range
  year <- as.character(2012:2018) 
  
  month <- as.character(01:12)
  month <- str_pad(month, 2, pad = "0")
  
  period <- ""
  for (i in 2012:2018)
  {
    period <- c(period, paste0(i,month))
  }
  
  period <- period[-1:-4]
  
  #create empty data frame
  forcasting <- data.frame(period, stringsAsFactors = F)
  
  #Ensure names in DataFrame are correct
  names(DFforTimeSeries) <- c("period", "data")
  
  #-----Add data
  forcasting <- merge(forcasting, DFforTimeSeries, by='period', all.x=T)
  
  names(forcasting) <- c("period", "data")
  
  forcasting$year <- substr(forcasting$period, 1,4) %>% as.numeric()
  forcasting$month <- substr(forcasting$period, 5,6) %>% as.numeric()
  return(forcasting)
}

#create rolling forecast
createForecastPrediction <- function(forcasting){
  var <- nrow(na.omit(forcasting[2])) -1
  
  for (i in 1:35){
    s <- i
    e <- ifelse(i+36 > var,var, i+36)
    new <- timeSeries(forcasting$data[s:e], 
                      forcasting$year[s], 
                      forcasting$month[s], 
                      forcasting$year[e], 
                      forcasting$month[e], 
                      'housing_advice')
    new <- forecasting(new)
    names(new) <- c('period', i)
    new <- new[1:16,]
    forcasting <- merge(forcasting, new, by='period', all.x=T)
  }
  
  forcasting$average <- NA
  
  
  for (i in 38:81){
    forcasting$average[i] <- forcasting[i,5:39]  %>% unlist(.) %>% mean(., na.rm=TRUE)
  }
  
  ggpredict <- forcasting[c('period','data','average')]
  names(ggpredict) <- c('period', 'Actual Data', 'Rolling Prediction')
  ggpredict$`Rolling Prediction` <- round(ggpredict$`Rolling Prediction`, 0)
  return(ggpredict)
}

#carry out time series analysis, uses above two methods
carryOutTimeSeries <- function(DFforTimeSeries){
  
  forcastingDF <- createTimeSeriesDataFrame(DFforTimeSeries)
  forcastingDF <- createForecastPrediction(forcastingDF)
  return(forcastingDF)
  
}


#create a forecast plot
ggPlotForecast <- function(forcastingDF, varName){
  
  ggpredict <- gather(forcastingDF, "type", "count", 2:3)
  
  from <- paste0(substr(min(ggpredict$period), 5, 6), "-",  substr(min(ggpredict$period), 1, 4))
  to <- paste0(substr(max(ggpredict$period), 5, 6), "-",  substr(max(ggpredict$period), 1, 4))
  
  gg1 <- ggplot(ggpredict, aes(x=factor(period), y=count, group=type, colour=type)) + 
    geom_line() + 
    geom_smooth() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8)) +
    labs(title=paste0(varName,", between ", from, " and ", to,"\nPlus rolling forecast"),
         x ="Month", y = paste0("Count"))
  #filename <- paste0("E:\\Michael\\Comsol\\Charts\\",varName,", between ", from, " and ", to, ".jpg")
  #ggsave(filename, dpi = 300, units = "in", width = 12, height = 8)
  return(gg1)
}
