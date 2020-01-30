#Loading in the Dataset 
folder <- "C:/Users/User/Documents/University/Honours Project/Air Quality/"
 file_list <- list.files(path = folder, pattern = "*.csv")

 for (i in 1:length(file_list)){
   assign(file_list[i], 
   read.csv(paste(folder, file_list[i], sep=''))
   )}
 
 #Simple Data Exploration 
 nrow(Aotizhongxin_Station.csv)
 