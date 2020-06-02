#  Course          : Knowledge Discovery & Data Mining
#  First Name      : Aakash
#  Last Name       : Rami
#  CWId            : 10453138
#Midterm Examination

rm(list=ls())

#Loading file into csvfile
choosecsv<-file.choose()
covid_dataset<- read.csv(choosecsv,header=TRUE,na.strings = " ")

View(covid_dataset)
# Summarizing each column
summary(covid_dataset)

# Identify Missing Values 
missing_values <- which(is.na(covid_dataset))
missing_values


# Displaying the frequency table of Infected vs. MaritalStatus
freqtable <- table(covid_dataset$Infected, covid_dataset$MaritalStatus)
ftable(freqtable)

# Displaying the scatter plot of Age,MaritalStatus & MonthatHospital one pair at a time
plot(covid_dataset[,-c(1,3,5,7)], main = "Scatter Plot of Age, MaritalStatus & MonthatHospital", ph = 10, col = 2)

# Show histogram box plot for Age,MaritalStatus & MonthatHospital
boxplot(covid_dataset[,-c(1,3,5,7)], main = "Histogram Box Plot of Age, MaritalStatus & MonthatHospital")

# Replacing Missing values of Cases with mean of Cases
covid_dataset[is.na(covid_dataset[,5]),5] <- mean(covid_dataset[,5], na.rm = TRUE)
View(covid_dataset)
