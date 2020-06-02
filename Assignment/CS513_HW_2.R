#  Course          : Data Mining
#  First Name      : Aakash   
#  Last Name       : Rami
#  CWId            : 10453138
#  purpose         : Homework Assignment 2

rm(list=ls())

breast_cancer <- read.csv("/Users/aakash/Desktop/CS 513/breast-cancer-wisconsin.data.csv", na.string = "?")

View(breast_cancer)
# Summarizing each column
summary(breast_cancer)

# Identify Missing Values 
is.na(breast_cancer)
View(breast_cancer)

# Replacing Missing values with mean of columns
for(i in 1:ncol(breast_cancer)){
  breast_cancer[is.na(breast_cancer[,i]), i] <- mean(breast_cancer[,i], na.rm = TRUE)
}

View(breast_cancer)
# Displaying the frequency table of “Class” vs. F6
freqtable <- table(breast_cancer$Class, breast_cancer$F6)
ftable(freqtable)

# Displaying the scatter plot of F1 to F6, one pair at a time
plot(breast_cancer[2:7], main = "Scatter Plot of F1 to F6", ph = 10, col = 2)

# Show histogram box plot for columns F7 to F9
boxplot(breast_cancer[8:10], main = "Histogram Box Plot for Columns F7 to F9")

rm(list=ls())

breast_cancer <- read.csv("/Users/aakash/Desktop/CS 513/breast-cancer-wisconsin.data.csv", na.string = "?")

breast_cancer[,1:11][breast_cancer[,1:11]=="?"] <- NA
breast_cancer <- na.omit(breast_cancer)
