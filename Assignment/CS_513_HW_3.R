#  Course          : Data Mining
#  First Name      : Aakash   
#  Last Name       : Rami
#  CWId            : 10453138
#  purpose         : Homework Assignment 3

install.packages("e1071")
install.packages("class")

library(e1071)
library(class)

breast_dataset <- read.csv("/Users/aakash/Desktop/CS 513/breast-cancer-wisconsin.data.csv", na.string = "?")

breast_can_dataset_without_na <- na.omit(breast_dataset)

View(breast_can_dataset_without_na)

##Define max-min normalization function
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}


breastcancer_normalized<-as.data.frame (         
  cbind(  F1=mmnorm(breast_can_dataset_without_na[,2],min(breast_can_dataset_without_na[,2]),max(breast_can_dataset_without_na[,2]))
          ,F2=mmnorm(breast_can_dataset_without_na[,3],min(breast_can_dataset_without_na[,3]),max(breast_can_dataset_without_na[,3]))
          ,F3=mmnorm(breast_can_dataset_without_na[,4],min(breast_can_dataset_without_na[,4]),max(breast_can_dataset_without_na[,4]))
          ,F4=mmnorm(breast_can_dataset_without_na[,5],min(breast_can_dataset_without_na[,5]),max(breast_can_dataset_without_na[,5]))
          ,F5=mmnorm(breast_can_dataset_without_na[,6],min(breast_can_dataset_without_na[,6]),max(breast_can_dataset_without_na[,6]))
          ,F6=mmnorm(breast_can_dataset_without_na[,7],min(breast_can_dataset_without_na[,7]),max(breast_can_dataset_without_na[,7]))
          ,F7=mmnorm(breast_can_dataset_without_na[,8],min(breast_can_dataset_without_na[,8]),max(breast_can_dataset_without_na[,8]))
          ,F8=mmnorm(breast_can_dataset_without_na[,9],min(breast_can_dataset_without_na[,9]),max(breast_can_dataset_without_na[,9]))
          ,F9=mmnorm(breast_can_dataset_without_na[,10],min(breast_can_dataset_without_na[,10]),max(breast_can_dataset_without_na[,10]))
          ,Class=as.character(breast_can_dataset_without_na[,11])
          
  )
)

data_fact <- colnames(breastcancer_normalized)
breastcancer_normalized[data_fact] <- lapply(breastcancer_normalized[data_fact], factor)

split_data <- sort(sample(nrow(breastcancer_normalized), as.integer(.70*nrow(breastcancer_normalized))))

test_data<-breast_can_dataset_without_na[split_data,]
train_data <-breast_can_dataset_without_na[-split_data,]
library(kknn)
?kknn()

breast_can_predict_k3 <- kknn(formula=Class~., train_data, test_data[,-11], k=3,kernel ="triangular" )
fit <- fitted(breast_can_predict_k3)
table(Actual=test_data$Class,Fitted=fit)

breast_can_predict_k5 <- kknn(formula=Class~., train_data, test_data[,-11], k=5,kernel ="triangular" )
fit <- fitted(breast_can_predict_k5)
table(Actual=test_data$Class,Fitted=fit)

breast_can_predict_k10 <- kknn(formula=Class~., train_data, test_data[,-11], k=10,kernel ="triangular" )
fit <- fitted(breast_can_predict_k10)
table(Actual=test_data$Class,Fitted=fit)


