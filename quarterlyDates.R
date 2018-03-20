library(zoo)


table(homelessDataLatestNumbersDF$quarter)

with(homelessDataRawDF, table(appreason, quarter))

head(homelessDataRawDF)     



new_data <- read.csv("X:\\Comsol\\new csv\\AC - HLN Decision Made Cases.csv", stringsAsFactors = F)

new_data$Application.Date <- dmy(new_data$Application.Date)
new_data$period <- period(new_data$Application.Date)


df <- table(new_data$period) %>% as.data.frame() %>% merge(., monthlyValues, by.x="Var1",by.y="period", all.y=T)

gather(df[1:3], key = "type", "value", 2:3) %>% 
  ggplot(aes(x= factor(Var1), y=value, group=type, colour=type)) +
  geom_line() + geom_smooth()



monthlyValues[59:81,]

df <- df[1:70,]

new_homeless_Forecasting <- carryOutTimeSeries(df[c(1:2)])
new_homeless_Plot <- ggPlotForecast(new_homeless_Forecasting, "Homeless Applications")
new_homeless_Plot

ts <- timeSeries(df[[2]], 2012,04,2018,02,"Homeless Applications")
plot(ts)

