#  Course          : Knowledge Discovery And Data Mining
#  First Name      : Aakash   
#  Last Name       : Rami
#  CWId            : 10453138
#  purpose         : Homework Assignment 6

##Load Libraries

library(C50)

data_file <- file.choose()
breast_cancer <- read.csv(data_file, na.string = "?")
View(breast_cancer)

breast_cancer$Class<-factor(breast_cancer$Class) 

##Train and Test Data Preperation
train <- sort(sample(nrow(breast_cancer), size = floor(.70*nrow(breast_cancer)), replace = F)) 
train_data <- breast_cancer[train,] #Split train Data
test_data <-  breast_cancer[-train,] #Split test Data


##Model Building and Prediction
model<-C5.0(Class~., data = train_data, method = "class")
prediction_data<-predict(model,test_data,type="class")
plot(model)
table(test_data$Class,prediction_data)
wrong_data <- sum(prediction_data!=test_data$Class)
wrong_data
error_rate <- wrong_data/length(prediction_data)
error_rate