#  Course          : Knowledge Discovery And Data Mining
#  First Name      : Aakash   
#  Last Name       : Rami
#  CWId            : 10453138
#  purpose         : Homework Assignment 8  

rm(list=ls())
library(cluster)
library(fpc)

data_file <- file.choose()
breast_cancer <- read.csv(data_file, na.string = "?")
View(breast_cancer)

#Data Preperation
breast_cancer<-breast_cancer[-1]
breast_cancer<-na.omit(breast_cancer)#Remove any row with a missing value in any of the columns.
breast_cancer_pred<-breast_cancer[1]
View(breast_cancer_pred)
breast_cancer <-data.frame(lapply(breast_cancer[,-1],as.numeric))
View(breast_cancer)

hclust<-dist(breast_cancer)
hclust_resutls<-hclust(hclust)
plot(hclust_resutls)
hclust_2<-cutree(hclust_resutls,2)
View(hclust_2)
table(hclust_2,t(breast_cancer_pred))
plotcluster(breast_cancer,hclust_2)

#Kmeans Clustering
breast_cancer<-read.csv(data_file,na.strings = ' ?')
breast_cancer<-breast_cancer[,-1]
kmeans_2<- kmeans(breast_cancer[,-1],2,nstart = 10)
kmeans_pred <- kmeans_2$cluster
table(kmeans_pred,breast_cancer[,1])
plotcluster(breast_cancer[-1],kmeans_pred)