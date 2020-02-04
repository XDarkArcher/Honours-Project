#Libraries
library(ggplot2)

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
ggplot(Aotizhongxin_Station.csv, aes(x=month)) + geom_histogram(binwidth = 1)

barplot(table(Aotizhongxin_Station.csv$month))

boxplot(Aotizhongxin_Station.csv$PM10)

qplot(Aotizhongxin_Station.csv$day, geom = "histogram")

ggplot(Aotizhongxin_Station.csv, aes(x=PM2.5)) + geom_histogram()+theme_bw()

 