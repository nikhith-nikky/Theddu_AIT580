###------------------
###Regression and Clustering
###------------------

###Students Name: Nikhith Theddu
###GNumber: G01225155


rm(list=ls())



library(ggplot2)

data <- read.csv('Theddu_AIT580/AIT580-master/data/EmployeeAttrition.csv')

# Your answers here...
#1.
#a) Show the scatter plot with relationship curve between TotalWorkingYears and MonthlyIncome. Briefly explain your observation in the plot
scatter.smooth(data$TotalWorkingYears,data$MonthlyIncome,
               main="WORKINGYEARS vs MONTHLYINCOME", span= 3/2, degree =1,
               xlab="TOTAL WORKING YEARS", ylab = "MONTHLY INCOME",
               family= c("symmetric", "gaussian"),lpars = list(col= "red",
                                                               lwd = 4, lty = 5))
#The smooth curve does not fit the scatter plot. Monthly Income is directly proportional to the Total Working Years, i.e., the increase in Total Working Years increases the Monthly Income. 
#b)Show the scatter plot with relationship curve between Age and DistanceFromHome.Briefly explain your observation in the plot
scatter.smooth(data$Age,data$DistanceFromHome,
               main="AGE vs DISTANCE FROM HOME", span= 3/2, degree =1,
               xlab="Age", ylab = "Distance from Home",
               family= c("symmetric", "gaussian"),lpars = list(col= "green",
                                                               lwd = 4, lty = 5))
#The age and the distance from home have no correlation. They are not related to each other. Therefore, any change in one variable, does not impact the other.
#c)Calculate Correlation for (a) and (b) and explain the values to support your answer in (a) and (b) 
cor.test(data$TotalWorkingYears,data$MonthlyIncome)
cor.test(data$Age,data$DistanceFromHome)
#The value of correlation coefficient signifies the strength of the relation between any two variables. The value of a correlation coefficient lies in the range (-1, 1) where, -1 indicates high negative correlation, 1 indicates high positive correlation and 0 indicates that the variables are not related.
#In part (a), the correlation coefficient value is approximately 0.78 which means the two variables total working years and monthly income are highly correlated. In part (b), the correlation coefficient value is approximately zero with a negative sign indicating that the variables age and distance from home have least correlation.

#d)Using Linear Regression, find details of the relationship between TotalWorkingYears and MonthlyIncome. Explain results in terms of p-value at 95% confidence interval and determine whether the relationship is significant or not (Hint: Use lm() to create linear regression model.
data1<-lm(data$TotalWorkingYears ~ data$MonthlyIncome, data = data)
print(data1)
summary(data1)
#The p-value is very small. Therefore, we can say that the data is sufficiently inconsistent and null hypothesis can be rejected at 95% confidence interval.
#2.
data_df<-data.frame(data$HourlyRate,data$TotalWorkingYears)
data_df
datak3<-kmeans(data_df,3)
datak3
datak5<-kmeans(data_df,5)
datak5
kmean_result <- data.frame(data$HourlyRate,data$TotalWorkingYears,datak3$cluster,datak5$cluster)
kmean_result
#a)Use K-means Clustering algorithm to find groups between HourlyRate and TotalWorkingYears. Use number of clusters as 3. Explain how each group is different from another in terms of employees representing those groups
plotk3<- ggplot(kmean_result, aes(kmean_result$data.HourlyRate,kmean_result$data.TotalWorkingYears,color=kmean_result$datak3.cluster))+geom_point()
plotk3
#3 clusters are formed based on the hourly rate. The clusters are clearly separated without any interference with each other. The number of employees in each cluster is different. There are 497, 467 and 506 employees in each cluster. Maximum number of employees are present in the cluster with highest value of hourly rate.

#b)Use number of clusters as 5. What did you observe? Did you see any split of groups observed in (a)? Observe the splitting groups and explain in terms of employees representing those groups.
plotk5<- ggplot(kmean_result, aes(kmean_result$data.HourlyRate,kmean_result$data.TotalWorkingYears,color=kmean_result$datak5.cluster))+geom_point()
plotk5
#There are 5 clusters with 343, 296, 349, 141 and 341 employees. All the clusters formed in part(a) are split to form new clusters with different set of employees having different values of centroid. These clusters do not have a clear boundary and interfere with other clusters.