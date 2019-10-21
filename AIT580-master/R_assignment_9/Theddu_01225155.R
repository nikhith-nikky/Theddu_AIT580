###------------------
###Hypothesis Testing
###------------------

###Students Name: NIKHITH THEDDU
###GNumber:01225155

setwd("Theddu_AIT580/AIT580-master/data")

rm(list=ls())

data <- read.csv('EmployeeAttrition.csv')


# Your hypothesis testings here...
# 1.If the MonthlyIncome of Males is greater than Females 
    Male_data= which(data$Gender=='Male')
    Female_data= which(data$Gender=='Female')
    t.test(data$MonthlyIncome[Male_data],data$MonthlyIncome[Female_data],alternative="greater", var.equal=T)
    #Null hypothesis cannot be rejected as p value is 0.88 which is greater than 0.05 at 95% confidence. Hence, we can assume null hypothesis that is the monthly income of males and females are equal.
    
#2.If the WorkLifeBalance of Males is less than Females
    Male_data= which(data$Gender=='Male')
    Female_data= which(data$Gender=='Female')
    t.test(data$WorkLifeBalance[Male_data],data$WorkLifeBalance[Female_data],alternative="less",var.equal=T)
    # Null hypothesis cannot be rejected as p value is 0.45 which is greater than 0.05 at 95% confidence. Hence, we can assume null hypothesis that is the WorkLifeBalance of males and females are equal.

#3.If the YearsAtCompany of Single is less than Married
    Single_data=which(data$MaritalStatus=='Single')
    Married_data=which(data$MaritalStatus=='Married')
    t.test(data$YearsAtCompany[Single_data],data$YearsAtCompany[Married_data],
           alternative = "less")
    #Null hypothesis is rejected as p value is 0.0049 which is less than 0.05 at 95% confidence. Hence, we can assume alternative hypothesis that is years at company is lesser in single employees as compared to married employees.
 
#4.If the EnvironmentalSatisfaction of Attrition=Yes is less than Attrition=No
    Attrition_data=which(data$Attrition=='Yes')
    Attrition_data1=which(data$Attrition=='No')
    t.test(data$EnvironmentSatisfaction[Attrition_data],data$EnvironmentSatisfaction[Attrition_data1],alternative = "less")
    #Null hypothesis is rejected as p value is 0.00010 which is less than 0.05 at 95% confidence. Hence, we can assume alternative hypothesis that is Environment satisfaction is lesser for employee attrition than those with no attrition

#5.If the MonthlyIncome of Manager is greater than Lab
    Manager_data=which(data$JobRole=='Manager')
    Technician_data=which(data$JobRole=='Laboratory Technician')
    t.test(data$MonthlyIncome[Manager_data],data$MonthlyIncome[Technician_data],
           alternative="greater")
    #Null hypothesis is rejected as p value is less than 0.05 at 95% confidence. Hence, we can assume alternative hypothesis that the manager has a higher monthly income than lab technician

#6.If YearsAtCompany and DailyRate are correlated with each other
    cor.test(data$YearsAtCompany,data$DailyRate)
    #Null hypothesis cannot be rejected as p value is 0.191 which is greater than 0.05 at 95% confidence.Hence, we can assume null hypothesis
    
#7.If YearsAtCompany and MonthlyIncome are correlated with each other
    cor.test(data$YearsAtCompany,data$MonthlyIncome)
    #Null hypothesis is rejected as p value is less than 0.05 at 95% confidence.Hence, we can assume alternate hypothesis
    
#8.If YearsAtCompany varies depending on individual's MaritalStatus
    summary(aov(data$YearsAtCompany ~ data$MaritalStatus))
    #Null hypothesis is rejected as p value is 0.0247 which is less than 0.05 at 95% confidence.Hence, we can assume alternate hypothesis

#9.If MonthlyIncome varies depending on individual's PerformanceRating
    summary(aov(data$MonthlyIncome ~ data$PerformanceRating))
    #Null hypothesis cannot be rejected as p value is 0.191 which is greater than 0.05 at 95% confidence.Hence, we can assume null hypothesis
    
#10.If MonthlyIncome varies depending on individual's WorkLifeBalance
    summary(aov(data$MonthlyIncome ~ data$WorkLifeBalance))
    #Null hypothesis cannot be rejected as p value is 0.191 which is greater than 0.05 at 95% confidence.Hence, we can assume null hypothesis
    
    