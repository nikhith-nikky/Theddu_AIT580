###------------------
###Visualization
###------------------

###Students Name:Nikhith Theddu
###GNumber:01225155


rm(list=ls())

data <- read.csv('Theddu_AIT580/AIT580-master/data/EmployeeAttrition.csv')
library(tidyverse)
#a. Create Histogram for Age using R
ggplot(data, aes(Age))+ggtitle("Histogram for Age")+geom_bar(width=0.8,color='black',fill='blue')
# ggplot is the package used to create this histogram with aes as age, it is named using ggtitle,width of the bar is defined as 0.8 and the fill as blue.
# The plot counts the employees with different ages, it can be observed that maximum number of employees are of age 35 and minimum number of employees are of age 57.
#c. Create Scatter Plot for Age and Monthly Income using R 
ggplot(data,aes( Age, MonthlyIncome))+ggtitle("Scatter plot for Age and Monthly Income")+geom_point(color='red')
# ggplot is the package used to create this scatter plot with aes as age and monthlyincome, it is named using ggtitle, red colour is assigned using geom_point.
# The plot describes the monthly income of people of different ages, it can be observed that 2,00,000 salary can be expected for the people of age more than 40.