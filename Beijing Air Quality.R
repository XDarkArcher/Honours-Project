#Libraries
library(ggplot2)
library(lubridate)
library(dplyr)
library(magrittr)
library(forecast)
library(corrplot)
library(Hmisc)

#Obtaining the data step
#Loading in the Dataset
folder <- "C:/Users/User/Documents/Honours-Project/Air Quality/"
file.list <- list.files(path = folder, pattern = "*.csv")

 for (i in 1:length(file.list)){
   assign(file.list[i], 
   read.csv(paste(folder, file.list[i], sep=''))
   )}
 
#Exploration Step
#Simple Data Exploration 

#Get the number of Rows and Columns
nrow(Aotizhongxin_Station.csv)
ncol(Aotizhongxin_Station.csv)

#Get the column names  
names(Aotizhongxin_Station.csv)
names(Changping_Station.csv)
 
#Show the first 10 rows in the dataframe
Aotizhongxin_Station.csv[0:9,]
Changping_Station.csv[0:9,]

#Summary of the Aotizhongxin monotoring station 
summary(Aotizhongxin_Station.csv)
summary(Changping_Station.csv)

#Structure of the data of on of the Stations
str(Aotizhongxin_Station.csv)

#Counts all the missing values for each Column
colSums(is.na(Aotizhongxin_Station.csv))
colSums(is.na(Changping_Station.csv))
colSums(is.na(Dingling_Station.csv))
colSums(is.na(Dongsi_Station.csv))
colSums(is.na(Guanyuan_Station.csv))
colSums(is.na(Gucheng_Station.csv))
colSums(is.na(Huairou_Station.csv))
colSums(is.na(Nongzhanguan_Station.csv))
colSums(is.na(Shunyi_Station.csv))
colSums(is.na(Tiantan_Station.csv))
colSums(is.na(Wanliu_Station.csv))
colSums(is.na(Wanshouxigong_Station.csv))

#Simple Visualisation of current trend

#Plot Attempts that did not look right

barplot(table(Aotizhongxin_Station.csv$month))

boxplot(Aotizhongxin_Station.csv$PM10)

qplot(Aotizhongxin_Station.csv$day, geom = "histogram")

ggplot(Aotizhongxin_Station.csv, aes(x=TEMP)) + geom_histogram()+theme.bw()

ggplot (Aotizhongxin_Station.csv, aes(x=year, y=PM2.5)) +
   geom_line()

ggplot (Aotizhongxin_Station.csv, aes(x=year, y=PM2.5)) +
   geom_boxplot(alpha=0.7) +
   stat.summary(fun.y = mean,geom="point", size=0.5,color="red")

ggplot (Aotizhongxin_Station.csv,aes(x=date,y=PM2.5)) +
   geom_point(size=0.5) +
   facet_wrap(~ year)


Aotizhongxin.month.PM2.5 <- Aotizhongxin.Station.csv %>%
   group.by (year,month) %>%
   summarise(max.PM2.5 = sum(PM2.5))

#Successful plot attempt

ggplot(Aotizhongxin_Station.csv, aes(x=month)) + geom_bar()

ggplot(Aotizhongxin_Station.csv, aes(x=month)) + geom_bar() +
   facet_wrap(~ year)

#Need to Merge the dates which is done after cleaning the data
ggplot (data = Aotizhongxin_Station.csv,aes(x=date,y=PM2.5))+
   geom_point(size = 0.1)

ggplot (data = Aotizhongxin_Station.csv,aes(x=date,y=PM10))+
   geom_point(size = 0.1)

#Cleaning Step
#Creating copy of the dataframe 
Aotizhongxin.Copy <- Aotizhongxin_Station.csv 

#Cleaning the Data using the average 
Aotizhongxin.Copy$PM2.5[is.na(Aotizhongxin.Copy$PM2.5)] <- mean(Aotizhongxin.Copy$PM2.5, na.rm = TRUE)
Aotizhongxin.Copy$PM10 [is.na(Aotizhongxin.Copy$PM10)]  <- mean(Aotizhongxin.Copy$PM10, na.rm = TRUE)
Aotizhongxin.Copy$SO2  [is.na(Aotizhongxin.Copy$SO2)]   <- mean(Aotizhongxin.Copy$SO2, na.rm = TRUE)
Aotizhongxin.Copy$NO2  [is.na(Aotizhongxin.Copy$NO2)]   <- mean(Aotizhongxin.Copy$NO2, na.rm = TRUE)
Aotizhongxin.Copy$CO   [is.na(Aotizhongxin.Copy$CO)]    <- mean(Aotizhongxin.Copy$CO, na.rm = TRUE)
Aotizhongxin.Copy$O3   [is.na(Aotizhongxin.Copy$O3)]    <- mean(Aotizhongxin.Copy$O3, na.rm = TRUE)
Aotizhongxin.Copy$TEMP [is.na(Aotizhongxin.Copy$TEMP)]  <- mean(Aotizhongxin.Copy$TEMP, na.rm = TRUE)
Aotizhongxin.Copy$PRES [is.na(Aotizhongxin.Copy$PRES)]  <- mean(Aotizhongxin.Copy$PRES, na.rm = TRUE)
Aotizhongxin.Copy$DEWP [is.na(Aotizhongxin.Copy$DEWP)]  <- mean(Aotizhongxin.Copy$DEWP, na.rm = TRUE)
Aotizhongxin.Copy$RAIN [is.na(Aotizhongxin.Copy$RAIN)]  <- mean(Aotizhongxin.Copy$RAIN, na.rm = TRUE)
Aotizhongxin.Copy$WSPM [is.na(Aotizhongxin.Copy$WSPM)]  <- mean(Aotizhongxin.Copy$WSPM, na.rm = TRUE)
summary(Aotizhongxin.Copy)
colSums(is.na(Aotizhongxin.Copy))
str(Aotizhongxin.Copy)

#Merging the sperate date elements as one date column 
paste(Aotizhongxin.Copy$year,Aotizhongxin.Copy$month,Aotizhongxin.Copy$day, sep = "-")
Aotizhongxin.Copy$date <- ymd( paste(Aotizhongxin.Copy$year,Aotizhongxin.Copy$month,Aotizhongxin.Copy$day, sep = "-"))
head(Aotizhongxin.Copy)

#Some More Exploration after cleaning 
#Summary after cleaning 
summary(Aotizhongxin.Copy)

#Finding any correlation between variables in the columns
Aotizhongxin.Copy.cor <- Aotizhongxin.Copy[-c(1:5,16,18:19)]
Aotizhongxin.Correlation = cor(Aotizhongxin.Copy.cor)

Aotizhongxin.rcorr = rcorr(as.matrix(Aotizhongxin.Copy.cor))
Aotizhongxin.rcorr

corrplot(Aotizhongxin.Correlation)

#Plot After cleaning
ggplot (data = Aotizhongxin.Copy,aes(x=date,y=PM2.5))+
   geom_point(size = 0.1)

ggplot(Aotizhongxin.Copy,aes(date,PM2.5)) + geom_point() + facet_wrap( ~ month) + ylab("Daily PM2.5 Levels for Aotizhongxin Satation") 

#Modelling the data step
#Creation of Time Series Object and plotting using seasonal frequency 

#Hourly Daily Seaonal Period of Aotizhongxin variables 
Aotizhongxin.PM2.5.ts <- ts(Aotizhongxin.Copy$PM2.5, frequency = 8766, start=c(2013,3,1), end=c(2017,2,28))
plot.ts(Aotizhongxin.PM2.5.ts)
tsdisplay(Aotizhongxin.PM2.5.ts, lag.max = 100)

Aotizhongxin.PM10.ts <- ts(Aotizhongxin.Copy$PM10, frequency = 8766, start=c(2013,3,1), end=c(2017,2,28))
plot.ts(Aotizhongxin.PM10.TS)

#52 Week Seaonal Period of Aotizhongxin 2.5 particulate 
Aotizhongxin.weekly.PM2.5.ts <- ts(Aotizhongxin.Copy$PM2.5,frequency = 52,start = c(2013,3,1), end=c(2017,2,28))
plot(Aotizhongxin.weekly.PM2.5.ts,main="Aotizhongxin Monitoring Station Weekly PM2.5",ylab="Weekly PM2.5 levels")
tsdisplay(Aotizhongxin.weekly.PM2.5.ts, lag.max = 100)

#Decomposing the time series
#24Hours Daily 
Aotizhongxin.PM2.5.Decomp = stl(Aotizhongxin.PM2.5.ts, s.window = "periodic")
Aotizhongxin.PM2.5.Deseasonal <- seasadj(Aotizhongxin.PM2.5.Decomp)
plot(Aotizhongxin.PM2.5.Decomp)

#52 Week Period
Aotizhongxin.weekly.PM2.5.Decomp = stl(Aotizhongxin.weekly.PM2.5.ts, s.window = "periodic")
Aotizhongxin.weekly.PM2.5.Deseasonal <- seasadj(Aotizhongxin.weekly.PM2.5.Decomp)
plot(Aotizhongxin.weekly.PM2.5.Decomp)

#ARIMA Models (run time may take a while)

#Auto arima using data without the data being decomposed
#Hourly Daily PM2.5 Model
Aotizhongxin.PM2.5.autoarima <- auto.arima(Aotizhongxin.PM2.5.ts)
summary(Aotizhongxin.PM2.5.autoarima)

#52 week period model
Aotizhongxin.weekly.PM2.5.autoarima <- auto.arima(Aotizhongxin.weekly.PM2.5.ts)
Aotizhongxin.weekly.PM2.5.autoarima
tsdisplay(residuals(Aotizhongxin.weekly.PM2.5.autoarima),main="52 Week Period PM2.5 autoarima model residuals for Aotizhongxin")
accuracy(Aotizhongxin.weekly.PM2.5.autoarima)

#Model Residuals
checkresiduals(Aotizhongxin.weekly.PM2.5.autoarima)

#Using decomposed data with auto arima function to see if there is any diiferences with or without decomposing

#Hourly Daily arima model
Aotizhongxin.PM2.5.fit <- auto.arima(Aotizhongxin.PM2.5.Deseasonal, seasonal = FALSE)
Aotizhongxin.PM2.5.fit
tsdisplay(residuals(Aotizhongxin.PM2.5.fit),lag.max = 60,main = '(0,1,5) Model Residuals')

Aotizhongxin.PM2.5.fit2 <- arima(Aotizhongxin.PM2.5.Deseasonal, order=c(0,1,17))
tsdisplay(residuals(Aotizhongxin.PM2.5.fit2),lag.max = 20,main = 'Aotizhongxin Daily Hourly PM2.5 Model Redsiduals')

#52 week period arima model
Aotizhongxin.weekly.PM2.5.fit <- auto.arima(Aotizhongxin.weekly.PM2.5.Deseasonal)
Aotizhongxin.weekly.PM2.5.fit
tsdisplay(residuals(Aotizhongxin.weekly.PM2.5.fit),main="52 Week Period PM2.5 autoarima model residuals for Aotizhongxin")

accuracy(Aotizhongxin.weekly.PM2.5.fit)

#Model Residuals
checkresiduals(Aotizhongxin.weekly.PM2.5.fit)

plot(Aotizhongxin.weekly.PM2.5.fit$residuals)
acf(Aotizhongxin.weekly.PM2.5.fit$residuals)
pacf(Aotizhongxin.weekly.PM2.5.fit$residuals)


#Forecasting the fit of the model

#Forcasting PM2.5 data
#Forecasts for the next month on a 24 hours basis with decompsed data
Aotizhongxin.PM2.5.fcast <- forecast(Aotizhongxin.PM2.5.fit,h=720)
plot(Aotizhongxin.PM2.5.fcast)

#Forecasts for the next month on a 24 hours basis without decompsing the data
#Aotizhonxing Hourly Daily PM2.5 forecast
Aotizhongxin.PM2.5.forecast <- forecast(Aotizhongxin.PM2.5.autoarima, h=720)
plot(Aotizhongxin.PM2.5.forecast)
plot(Aotizhongxin.PM2.5.forecast$residuals)
acf(Aotizhongxin.PM2.5.forecast$residuals)
pacf(Aotizhongxin.PM2.5.forecast$residuals)

#Forecasting the 52 week period model for the next year
#without decomposing
Aotizhongxin.weekly.PM2.5.forecast <- forecast(Aotizhongxin.weekly.PM2.5.autoarima, h=52)
plot(Aotizhongxin.weekly.PM2.5.forecast,main="52 Week Period forecast using ARIMA(2,1,3) for Aotizhongxin",sub="Without Decomposing",xlab="Time",ylab="Weekly PM2.5")
Aotizhongxin.weekly.PM2.5.forecast

#Residuals
plot(Aotizhongxin.weekly.PM2.5.forecast$residuals, main="Forecast Residuals",ylab="52 Week Period Residuals")
acf(Aotizhongxin.weekly.PM2.5.forecast$residuals)
pacf(Aotizhongxin.weekly.PM2.5.forecast$residuals)

Acf(Aotizhongxin.weekly.PM2.5.forecast$residuals)

attach(Aotizhongxin.weekly.PM2.5.forecast)
par(mfrow=c(3,1))
plot(Aotizhongxin.weekly.PM2.5.forecast$residuals, main="Forecast Residuals",ylab="52 Week Period Residuals")
Acf(Aotizhongxin.weekly.PM2.5.forecast$residuals, main="Forecast Autocorrelation Function")
Pacf(Aotizhongxin.weekly.PM2.5.forecast$residuals, main="Forecast Partial Autocorrelation Function", sub="Without Decomposing")

#Accuracy Check
accuracy(Aotizhongxin.weekly.PM2.5.forecast)

autoplot(forecast(Aotizhongxin.weekly.PM2.5.autoarima))

#With Decomposing
Aotizhongxin.weekly.PM2.5.forecast2 <- forecast(Aotizhongxin.weekly.PM2.5.fit, h=52)
plot(Aotizhongxin.weekly.PM2.5.forecast2,main="52 Week Period forecast using ARIMA(2,1,3) for Aotizhongxin",sub="With Decomposing",xlab="Time",ylab="Weekly PM2.5")
Aotizhongxin.weekly.PM2.5.forecast2

#Residuals
plot(Aotizhongxin.weekly.PM2.5.forecast2$residuals,main="Forecast Residuals",ylab="52 Week Period Residuals")
acf(Aotizhongxin.weekly.PM2.5.forecast2$residuals)
pacf(Aotizhongxin.weekly.PM2.5.forecast2$residuals)

attach(Aotizhongxin.weekly.PM2.5.forecast2)
par(mfrow=c(3,1))
plot(Aotizhongxin.weekly.PM2.5.forecast2$residuals,main="Forecast Residuals",ylab="52 Week Period Residuals")
Acf(Aotizhongxin.weekly.PM2.5.forecast2$residuals,main="Forecast Autocorrelation Function")
Pacf(Aotizhongxin.weekly.PM2.5.forecast2$residuals,  main="Forecast Partial Autocorrelation Function", sub="With Decomposing")

#Accuracy Check
accuracy(Aotizhongxin.weekly.PM2.5.forecast2)

autoplot(forecast(Aotizhongxin.weekly.PM2.5.fit))






 