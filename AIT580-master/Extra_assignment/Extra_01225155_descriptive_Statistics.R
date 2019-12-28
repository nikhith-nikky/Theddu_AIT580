library(tidyverse)
library(tidyr)
library(dplyr)
data<-read.csv('C:/Users/Owner/Downloads/reddit.csv')
mean(data$Number.of.comments) # average number of comments
new_data<- data %>% separate(timestamp,c("Date","Time"),sep = " ") #separate data and time
final_data<- new_data %>% separate(Date,c("Year","Month","Day"),sep = "-") #separate year, month,day
ggplot(final_data,aes(Year))+geom_bar() #plot of year and count
ggplot(final_data,aes(Month))+geom_bar() #plot of month and count
tail(names(sort(table(final_data$Author))),5) #prints top 5 users
final_data[which.max(final_data$Number.of.comments),] #prints row containing max number of comments
final_data[which.max(final_data$Points),] #prints row containing max points
summary(final_data) #summary of the whole data