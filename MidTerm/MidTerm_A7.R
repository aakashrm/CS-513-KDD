#  Course          : Knowledge Discovery & Data Mining
#  First Name      : Aakash
#  Last Name       : Rami
#  CWId            : 10453138
#Midterm Examination
rm(list=ls())
#Loading file into csvfile
choosecsv<-file.choose()
covid_data<- read.csv(choosecsv,header=TRUE,na.strings = " ")
#removing NA from the loaded csvfile
covid_dataset_without_na <- na.omit(covid_data)

View(covid_dataset_without_na)


#Define max-min normalization function
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}


covid_normalized<-as.data.frame (         
  cbind( ID=as.character(covid_dataset_without_na[,1])
         ,Age=as.character(covid_dataset_without_na[,2])
         , Exposure=mmnorm(covid_dataset_without_na[,3],min(covid_dataset_without_na[,3]),max(covid_dataset_without_na[,3]))
         ,MaritalStatus=as.character(covid_dataset_without_na[,4])         
         , Cases=mmnorm(covid_dataset_without_na[,5],min(covid_dataset_without_na[,5]),max(covid_dataset_without_na[,5]))
         ,MonthAtHospital=as.character(covid_dataset_without_na[,6])
         ,Infected=as.character(covid_dataset_without_na[,7])
         
  )
)
?sample()
?sort()
#factoring the column to be predicted
data_col <- colnames(covid_normalized)
covid_normalized[data_col] <- lapply(covid_normalized[data_col], factor)


#Dividing the normalized data in 70:30 ratio
split_data <- sort(sample(nrow(covid_normalized), as.integer(.70*nrow(covid_normalized))))


train_data <-covid_normalized[split_data,]
test_data<-covid_normalized[-split_data,]


#KNN MODEL
library(kknn)

?kknn()

covid_predict_k5 <- kknn(formula=Infected~., train_data, test_data[,-7], k=5,kernel ="rectangular" )
fit <- fitted(covid_predict_k5)
table(test_data$Infected,fit)



