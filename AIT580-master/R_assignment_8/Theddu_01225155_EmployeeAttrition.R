###--------------------------------------
#Student Name: NIKHITH THEDDU
#GNumber: 01225155
###--------------------------------------

rm(list=ls())

data <- read.csv('nikhith27/Theddu_AIT580/AIT580-master/data/EmployeeAttrition.csv')

# this is just for testing to use "print" statement.
print(data[1,])



# a. Find the number of rows and columns in the dataset (5 points)
     print(nrow(data))
     print(ncol(data))

# b. Find the maximum Age in the dataset (5 points)
     print(max(data$Age))

# c. Find the minimum DailyRate in the dataset (5 points)
     print(min(data$DailyRate))

# d. Find the average/mean MontlyIncome in the dataset (5 points)
     print(mean(data$MonthlyIncome))

# e. How many employees rated WorkLifeBalance as 1 (5 points)
     print(length(which((data$WorkLifeBalance==1))))
     
# f. What percent of total employees have TotalWorkingYears less than equal to 5? Also calculate the percentage for TotalWorkingYears greater than 5 (5 points)
     print((sum(data$TotalWorkingYears<=5)/nrow(data))*100)
     print((sum(data$TotalWorkingYears>5)/nrow(data))*100)

# g. Print EmployeeNumber, Department and MaritalStatus for those employees whose Attrition is Yes and RelationshipSatisfaction is 1 and YearsSinceLastPromotion is greater than 3 (10 points)
     data[(data$Attrition=='Yes') & (data$RelationshipSatisfaction==1) & 
            (data$YearsSinceLastPromotion>3),c(10,5,18)]

# h. Find the mean, median, mode, standard deviation and frequency distribution of EnvironmentSatisfaction for males and females separately. (Hint: For frequency distribution use table() function (10 points)
     males1<- subset(data, data$Gender== 'Male')
     females1<-subset(data, data$Gender== 'Female')
     mode = function(x){ unx=unique(x) 
     unx[which.max(tabulate(match(x,unx)))]}
     mean(males1$EnvironmentSatisfaction)
     median(males1$EnvironmentSatisfaction)
     mode(males1$EnvironmentSatisfaction)
     sd(males1$EnvironmentSatisfaction)
     summary(males1$EnvironmentSatisfaction)
     mean(females1$EnvironmentSatisfaction)
     median(females1$EnvironmentSatisfaction)
     mode(females1$EnvironmentSatisfaction)
     sd(females1$EnvironmentSatisfaction)
     summary(females1$EnvironmentSatisfaction)
     ftable(males1$EnvironmentSatisfaction)
     ftable(females1$EnvironmentSatisfaction)
     
