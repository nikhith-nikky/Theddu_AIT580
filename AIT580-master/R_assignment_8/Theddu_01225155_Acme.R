###--------------------------------------
#Student Name: NIKHITH THEDDU
#GNumber: 01225155
###--------------------------------------

rm(list=ls())

data <- read.csv('nikhith27/Theddu_AIT580/AIT580-master/data/Acme.csv')
library(ggplot2)

# 1.Identify data types for each attribute in the dataset 
typeof(data$Years)
typeof(data$StSalary)
typeof(data$Gender)
typeof(data$Degree)

# 2.Produce a summary statistic for each attribute in the dataset
summary(data$Years)
summary(data$StSalary)
summary(data$Gender)
summary(data$Degree)

# 3.Produce visualizations for each attribute
    hist(data$Years, col='blue', main='Histogram for Years')
    hist(data$StSalary, col='blue',main='Histogram for Salary')
    plot(data$Gender, col='green',main='Histogram for Gender')
    plot(data$Degree, col='green',main = 'Histogram for Degree')

# 4.Display the relationship between
#   a.Years of Experience and Starting Salary for all employees
      ggplot(data, aes(x=Years,y=StSalary))+geom_point()+
      ggtitle('Years of Experience and Starting Salary for all employees')
#   b.Years of Experience and Starting Salary for each gender
      ggplot(data, aes(x=Years, y=StSalary,color=Gender))+
      ggtitle('Years of Experience and Starting Salary for each gender')+geom_point()
#   c.Years of Experience and Starting Salary for each degree 
      ggplot(data,aes(x=Years,y=StSalary,shape=Degree,color=Degree))+
      geom_point()+ggtitle('Years of Experience and Starting Salary for each degree')
# 5.Find the correlation between Starting Salary and Years of Experience
#   a. Is the correlation different for each gender
      male<-subset(data,data$Gender=='M')
      female<-subset(data,data$Gender=='F')
      cor(female$Years, female$StSalary,method="pearson",use="complete.obs")
      cor.test(female$Years, female$StSalary)
      cor(male$Years, male$StSalary, method="pearson",use="complete.obs")
      cor.test(male$Years, male$StSalary)
#   b. Is the correlation different for each degree
      bs<-subset(data,data$Degree=='BS')
      ms<-subset(data,data$Degree=='MS')
      phd<-subset(data,data$Degree=='PhD')
      cor(bs$Years, bs$StSalary,  method = "pearson", use = "complete.obs")
      cor.test(bs$Years, bs$StSalary)
      cor(ms$Years, ms$StSalary,  method = "pearson", use = "complete.obs")
      cor.test(ms$Years, ms$StSalary)
      cor(phd$Years, phd$StSalary,  method = "pearson", use="complete.obs")
      cor.test(phd$Years, phd$StSalary)
# 6. What can you conclude about Acme with respect to gender bias after your overall analysis
      
      #The correlation coefficient for salary and years of experience for female employees 0.7507096 and 0.6738658 for male employees. 
      #The correlation coefficient is higher for female employees than male employees. This means that, the starting salary of a female employee is more dependent on the number of years of experience whereas for a male employee, the starting salary is not highly dependent on the number of years of experience. 
      #The correlation coefficient for BS is 0.3560727 , for MS is 0.4504873 , for Phd is 0.04563974
      
      
      
      