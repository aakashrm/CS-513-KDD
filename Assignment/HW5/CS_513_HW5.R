#  Course          : Data Mining
#  First Name      : Aakash   
#  Last Name       : Rami
#  CWId            : 10453138
#  purpose         : Homework Assignment 5

#install.packages("rpart")


rm(list=ls()) #clear the environment
##Load Libraries

library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
data_file <- file.choose()
breast_cancer <- read.csv(data_file, na.string = "?")
View(breast_cancer)

##Define max-min normalization function
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}


breastcancer_normalized<-as.data.frame (         
  cbind(  F1=mmnorm(breast_cancer[,2],min(breast_cancer[,2]),max(breast_cancer[,2]))
          ,F2=mmnorm(breast_cancer[,3],min(breast_cancer[,3]),max(breast_cancer[,3]))
          ,F3=mmnorm(breast_cancer[,4],min(breast_cancer[,4]),max(breast_cancer[,4]))
          ,F4=mmnorm(breast_cancer[,5],min(breast_cancer[,5]),max(breast_cancer[,5]))
          ,F5=mmnorm(breast_cancer[,6],min(breast_cancer[,6]),max(breast_cancer[,6]))
          ,F6=mmnorm(breast_cancer[,7],min(breast_cancer[,7]),max(breast_cancer[,7]))
          ,F7=mmnorm(breast_cancer[,8],min(breast_cancer[,8]),max(breast_cancer[,8]))
          ,F8=mmnorm(breast_cancer[,9],min(breast_cancer[,9]),max(breast_cancer[,9]))
          ,F9=mmnorm(breast_cancer[,10],min(breast_cancer[,10]),max(breast_cancer[,10]))
          ,Class=as.character(breast_cancer[,11])
          
  )
)

data_fact <- colnames(breastcancer_normalized)
breastcancer_normalized[data_fact] <- lapply(breastcancer_normalized[data_fact], factor)

split_data <- sort(sample(nrow(breastcancer_normalized),as.integer(.70*nrow(breastcancer_normalized))))

train_data <-breast_cancer[split_data,]

test_data<-breast_cancer[-split_data,]

train_data$Class=as.factor(train_data$Class)

model<-rpart(Class~.,train_data[,-1],method="class")
rpart.plot(model,type = 4,extra = 101,roundint = FALSE)
prediction_data<-predict(model,test_data[,-1],type="class")
table(test_data[,11],prediction_data)
wrong_pred<-sum(test_data[,11]!=prediction_data)
error_rate<-wrong_pred/length(test_data[,11])
error_rate #To print the error rate 

library(rpart.plot)
prp(model, roundint = FALSE)


# much fancier graph
fancyRpartPlot(model)

