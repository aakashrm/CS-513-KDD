#  Course          : Knowledge Discovery & Data Mining
#  First Name      : Aakash
#  Last Name       : Rami
#  CWId            : 10453138
#Midterm Examination

rm(list=ls())

#Loading file into csvfile
choosecsv<-file.choose()
covid_data<- read.csv(choosecsv,header=TRUE,na.strings = " ")


#removing NA from the loaded file i.e csvfile
covid_csv_without_NA<-na.omit(covid_data)


#taking max values from Age and MonthAthospital column for discretize
max_hp<-max(covid_csv_without_NA$MonthAtHospital)
max_age<-max(covid_csv_without_NA$Age)

#Discretizing the Age and MonthAthosplital into given labels
covid_csv_without_NA$MonthAtHospital<-cut(covid_csv_without_NA$MonthAtHospital,breaks=c(-1,6,max_hp),labels=c("less than 6 months","6 or more months"))
covid_csv_without_NA$Age<-cut(covid_csv_without_NA$Age,breaks=c(-1,35,50,max_age),labels=c("less than 35","35 to 50","51 or more"))
View(covid_csv_without_NA)


##Define max-min normalization function
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}
str(new_csv_withno_NA)
normalized_covid<-as.data.frame (         
  cbind( ID=as.character(covid_csv_without_NA[,1])
         ,Age=as.character(covid_csv_without_NA[,2])
         , Exposure=mmnorm(covid_csv_without_NA[,3],min(covid_csv_without_NA[,3]),max(covid_csv_without_NA[,3]))
         ,MaritalStatus=as.character(covid_csv_without_NA[,4])         
         , Cases=mmnorm(covid_csv_without_NA[,5],min(covid_csv_without_NA[,5]),max(covid_csv_without_NA[,5]))
         ,MonthAtHospital=as.character(covid_csv_without_NA[,6])
         ,Infected=as.character(covid_csv_without_NA[,7])
         
  )
)
?sample()
?sort()

#factoring the column to be predicted
data_column<-colnames(normalized_covid)
normalized_covid[data_column]<-lapply(normalized_covid[data_column], factor)

#Dividing the normalized data in 70:30 ratio
split_data<-sort(sample(nrow(normalized_covid),as.integer(.70*nrow(normalized_covid))))

train_data<-normalized_covid[split_data,]
test_data<-normalized_covid[-split_data,]
#install.packages('e1071', dependencies = TRUE)
library(e1071)
?naiveBayes

train_data$Infected=as.factor(train_data$Infected)

naive_bayes <- naiveBayes(Infected~., data=train_data)
#Naive Bayes 
predict_naive <- predict(naive_bayes,test_data,type="class")

#Confusion matrix
table(test_data$Infected,predict_naive)

#Calculating the accuracy
wrong_data<-sum(test_data[,7]!=predict_naive)
error_rate_acc<-wrong_data/length(test_data[,7])
error_rate_acc
