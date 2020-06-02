#  Course          :Knowledge Discovery And Data Mining
#  First Name      : Aakash   
#  Last Name       : Rami
#  CWId            : 10453138
#  purpose         : Homework Assignment 7

rm(list=ls())
library(neuralnet)

data_file <- file.choose()
breast_cancer <- read.csv(data_file, na.string = "?")
View(breast_cancer)

breast_cancer<-data.frame(lapply(breast_cancer[,-1],as.numeric)) #Convert dataset in numeric


breast_normalized <- as.data.frame(apply(breast_cancer[,1:ncol(breast_cancer)],2,function(x) (x - min(x))/(max(x)-min(x))))#Normalization
View(breast_normalized)
#Splitting in test and train
split <- seq (1,nrow(breast_normalized),by=5)
test_data<- breast_normalized[split,]
train_data<-breast_normalized[-split,]

model<- neuralnet( diagnosis~. ,train_data, hidden=5, exclude = NULL,threshold=0.01)
plot(model)

#Predicting the categories
netpred <-predict(model, test_data)
print(netpred)

pred_category <- ifelse(netpred<0.5,0,1)

table(Actual = test_data$diagnosis, Prediction = pred_category)

wrong<- (test_data$diagnosis!=pred_category)
error_rate<-sum(wrong)/length(wrong)
error_rate

