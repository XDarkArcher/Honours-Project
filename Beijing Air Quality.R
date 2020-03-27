#Libraries
library(ggplot2)
library(lubridate)
library(dplyr)
library(magrittr)

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
ggplot(Aotizhongxin_Station.csv, aes(x=month)) + geom_histogram(binwidth = 1)

barplot(table(Aotizhongxin_Station.csv$month))

boxplot(Aotizhongxin_Station.csv$PM10)

qplot(Aotizhongxin_Station.csv$day, geom = "histogram")

ggplot(Aotizhongxin_Station.csv, aes(x=TEMP)) + geom_histogram()+theme_bw()

ggplot (Aotizhongxin_Station.csv, aes(x=year, y=PM2.5)) +
   geom_line()

ggplot (Aotizhongxin_Station.csv, aes(x=year, y=PM2.5)) +
   geom_boxplot(alpha=0.7) +
   stat_summary(fun.y = mean,geom="point",shape=10,size=8,color="red")

ggplot (data = Aotizhongxin_Station.csv,aes(x=date,y=PM2.5))+
   geom_point()

ggplot (Aotizhongxin_Station.csv,aes(x=date,y=PM2.5)) +
   geom_point() +
   facet_wrap(~ year)


Aotizhongxin_month_PM2.5 <- Aotizhongxin_Station.csv %>%
   group_by (year,month) %>%
   summarise(max_PM2.5 = sum(PM2.5))

Aotizhongxin_month_PM2.5 %>%
   ggplot (aes(x = month, y = max_PM2.5)) +
      geom_bar(stat = "identity") +
   facet_wrap(~ year, ncol = 3)

#Creating copies of the dataset
Aotizhongxin_Station.csv_Copy
   
#Cleaning the Data using the average 
Aotizhongxin_Station.csv$PM2.5[is.na(Aotizhongxin_Station.csv$PM2.5)] <- mean(Aotizhongxin_Station.csv$PM2.5, na.rm = TRUE)
summary(Aotizhongxin_Station.csv)

#Split the dataset into training and testing sets
 