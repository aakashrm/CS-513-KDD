#  Course          : Knowledge Discovery And Data Mining
#  First Name      : Aakash   
#  Last Name       : Rami
#  CWId            : 10453138
#  purpose         : Final Exam

rm(list=ls())
#install.packages("C50")
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle
library(class)
library(C50)

#Data Preparation
data_file <- file.choose()
admission_cat<- read.csv(data_file,header = TRUE, na.strings='?')
admission_cat <-admission_cat[,-1]
data_factor <- colnames(admission_cat)
admission_cat[data_factor] <- lapply(admission_cat[data_factor], factor)

#Splitting Data
split<-sort(sample(nrow(admission_cat),round(.30*nrow(admission_cat))))
train_data <- admission_cat[-split,]
test_data <- admission_cat[split,]

#Convert variable admit into factor
train_data$ADMIT <- as.factor(train_data$ADMIT)

# C5.0 Model
model<-C5.0(ADMIT~., data = train_data, method = "class")
prediction_data<-predict(model,test_data,type="class")
plot(model)
table(test_data$ADMIT,prediction_data)
tab <- table(test_data$ADMIT,prediction_data)
#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
