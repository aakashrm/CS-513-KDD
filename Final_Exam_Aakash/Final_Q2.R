#  Course          : Knowledge Discovery And Data Mining
#  First Name      : Aakash   
#  Last Name       : Rami
#  CWId            : 10453138
#  purpose         : Final Exam


rm(list=ls())
#install.packages("randomForest")
library(randomForest)

# Choose CSV
data_file <- file.choose()
admission_cat<- read.csv(data_file,header = TRUE, na.strings=' ?')
View(admission_cat)
#Data Preparation
admission_cat <- admission_cat[,-1]
#View(admission_cat)
cols <- ncol(admission_cat)
cols
admission_cat[1:cols] <- lapply(admission_cat[1:cols], factor)
View(admission_cat)

#Splitting Data
split<-sort(sample(nrow(admission_cat),round(.30*nrow(admission_cat))))
train_data <- admission_cat[-split,]
test_data <- admission_cat[split,]

#Random Forest Model
fit <- randomForest( ADMIT~., data=train_data, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, test_data)
table(actual=test_data$ADMIT,Prediction)
tab<-table(actual=test_data$ADMIT,Prediction)

#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

