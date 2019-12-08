library("selectr")
library("xml2")
library("rvest")
library(dplyr)
library(readr)
url <- "https://nytimes.com"
webpage <- read_html(url)

# Assignment 15 Task 1: write scripts that extract "titles" and "news summary" of articles out of the scrapped data. 
# Then, print them out using "print()" statement.
titles <- html_nodes(webpage, "div h2")
without_tags <- gsub("<.*?>", "", titles) 
print(without_tags)
News_summary <- html_nodes(webpage, "div p")
without_tags1 <- gsub("<.*?>", "", News_summary) 
print(without_tags1)


# Assignment 15 Task 2: write scripts that oraganize your data as dataframe with column names, "title" and "news summary", respectively.
# Then, save this dataframe as a CSV file. Name it as "NYT_titles.csv". 
max.length <- max(length(without_tags),length(without_tags1))
titles = c(without_tags, rep(NA, max.length - length(without_tags)))
News_summary = c(without_tags1, rep(NA, max.length - length(without_tags1)))
nytimes<-data.frame(Titles = titles , News_summary = News_summary)
is.data.frame(nytimes)
str(nytimes)
write_csv(nytimes,"NYT_titles.csv")



# Assignment 15 Task 3: once you save the CSV file, commit and push it back to your repository (no R scripts involved for Task 3). 

