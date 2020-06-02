#  Course          : Knowledge Discovery And Data Mining
#  First Name      : Aakash   
#  Last Name       : Rami
#  CWId            : 10453138
#  purpose         : Final Exam


rm(list=ls())
library(cluster)
library(fpc)

#Choose CSV
data_file <- file.choose()
admission_file <- read.csv(data_file, na.string = "?")
View(admission_file)

#Data Preperation
admission_file<-admission_file[-1]
#Remove any row with a missing value in any of the columns
admission_file<-na.omit(admission_file)
admission_file_pred<-admission_file[1]
View(admission_file)

#Hierarchical Clustering
admission_two_cols<- admission_file[c(2,3)]
View(admission_two_cols)
hclust<-dist(admission_two_cols)
hclust_results<-hclust(hclust)
plot(hclust_results)
hclust_2<-cutree(hclust_results,2)
View(hclust_2)
table(hclust_2,t(admission_file_pred))
plotcluster(admission_two_cols,hclust_2)

#Kmeans Clustering
admission_file1<-read.csv(data_file,na.strings = ' ?')
admission_file1<-admission_file1[,-1]
admission_file1<-admission_file1[c(2,3)]
kmeans_2<- kmeans(admission_file1,2,nstart = 10)
kmeans_pred <- kmeans_2$cluster
table(kmeans_pred,admission_file1[,1])
plotcluster(admission_file1,kmeans_pred)

