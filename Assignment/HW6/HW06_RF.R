#  Course          : Knowledge Discovery And Data Mining
#  First Name      : Aakash   
#  Last Name       : Rami
#  CWId            : 10453138
#  purpose         : Homework Assignment 6

##Load Libraries
library(randomForest)
rm(list=ls())

filename <-file.choose()
breastCancer  <-  read.csv(filename, na.string=" ?")# Load the dataset from CSV file
View(breastCancer)# View breast cancer data

#Data Preperation
breastCancer<-breastCancer[,-1]
cols<-ncol(breastCancer)
breastCancer[1:cols]<-lapply(breastCancer[1:cols],factor)
index<-sort(sample(nrow(breastCancer),round(.25*nrow(breastCancer))))
train_data<-breastCancer[-index,]
test_data<-breastCancer[index,]

#Fitting the model
fit <- randomForest( Class~., data=train_data, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, test_data)
table(actual=test_data$Class,Prediction)

#Finding error rate
wrong<- (test_data$Class!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate
