#  Course          : Data Mining
#  First Name      : Aakash   
#  Last Name       : Rami
#  CWId            : 10453138
#  purpose         : Homework Assignment 4

install.packages("e1071")

library(e1071)
data_file <- file.choose()
breast_dataset <- read.csv(data_file, na.string = "?")

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

train_data <-breastcancer_normalized[split_data,]
test_data<-breastcancer_normalized[-split_data,]


#Naive Bayes Model, prediction and confusion matrix
naivebayes_model<- naiveBayes(Class ~ ., data = train_data)
naivebayes_predict <- predict(naivebayes_model, test_data,type="class")
(naivebayes_predict)
table(naivebayes_model = naivebayes_predict, test_data$Class)


