#Libraries
library(ggplot2)
library(lubridate)
library(dplyr)
library(magrittr)
library(forecast)

#Loading in the Dataset 
folder <- "C:/Users/User/Documents/Honours-Project/Air Quality/"
file_list <- list.files(path = folder, pattern = "*.csv")

 for (i in 1:length(file_list)){
   assign(file_list[i], 
   read.csv(paste(folder, file_list[i], sep=''))
   )}
 
#Simple Data Exploration 

#Get the number of Rows and Columns
nrow(Aotizhongxin_Station.csv)
ncol(Aotizhongxin_Station.csv)

#Get the column names  
names(Aotizhongxin_Station.csv)
 
#Show the first 10 rows in the dataframe
Aotizhongxin_Station.csv[0:9,]

#Summary of the Aotizhongxin monotoring station 
summary(Aotizhongxin_Station.csv)

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

#Merging the sperate date elements as one date column 
paste(Aotizhongxin_Station.csv$year,Aotizhongxin_Station.csv$month,Aotizhongxin_Station.csv$day, sep = "-")
Aotizhongxin_Station.csv$date <- ymd( paste(Aotizhongxin_Station.csv$year,Aotizhongxin_Station.csv$month,Aotizhongxin_Station.csv$day, sep = "-"))
head(Aotizhongxin_Station.csv)

#Simple Visualisation of current trend

#Plot Attempts that did not look right
ggplot(Aotizhongxin_Station.csv, aes(x=month)) + geom_histogram(binwidth = 1)

barplot(table(Aotizhongxin_Station.csv$month))

boxplot(Aotizhongxin_Station.csv$PM10)

qplot(Aotizhongxin_Station.csv$day, geom = "histogram")

ggplot(Aotizhongxin_Station.csv, aes(x=TEMP)) + geom_histogram()+theme_bw()

ggplot (Aotizhongxin_Station.csv, aes(x=year, y=PM2.5)) +
   geom_line()

ggplot (Aotizhongxin_Station.csv, aes(x=year, y=PM2.5)) +
   geom_boxplot(alpha=0.7) +
   stat_summary(fun.y = mean,geom="point", size=0.5,color="red")

ggplot (Aotizhongxin_Station.csv,aes(x=date,y=PM2.5)) +
   geom_point(size=0.5) +
   facet_wrap(~ year)


Aotizhongxin_month_PM2.5 <- Aotizhongxin_Station.csv %>%
   group_by (year,month) %>%
   summarise(max_PM2.5 = sum(PM2.5))

#Successful plot attempt
ggplot (data = Aotizhongxin_Station.csv,aes(x=date,y=PM2.5))+
   geom_point(size = 0.1)

ggplot (data = Aotizhongxin_Station.csv,aes(x=date,y=PM2.5))+
   geom_line(size = 0.1)

#Creating copy of the dataframe 
AotizhongxinCopy <- Aotizhongxin_Station.csv 

#Cleaning the Data using the average 
AotizhongxinCopy$PM2.5[is.na(AotizhongxinCopy$PM2.5)] <- mean(AotizhongxinCopy$PM2.5, na.rm = TRUE)
summary(AotizhongxinCopy)

#Plot After cleaning
ggplot (data = AotizhongxinCopy,aes(x=date,y=PM2.5))+
   geom_point(size = 0.1)

#Creation of Time Series Object and plotting using seasonal frequency 
Aot_PM2.5_TS <- msts(AotizhongxinCopy$PM2.5, seasonal.periods = c(24,7*24, 365*24), start=c(2013,3,1), end=c(2017,2,28))
plot.ts(Aot_PM2.5_TS)
Aot_PM2.5_Sub = window(Aot_PM2.5_TS, start=c(2013,3), end=c(2017,2))
plot(Aot_PM2.5_Sub)
 