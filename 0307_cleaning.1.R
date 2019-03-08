rm(list = ls())
library(jsonlite)
library(stringr)
library(plyr)
library(data.table)
library(tidyverse)
library(xgboost)
library(stringi)
library(psych)
library(caret)
library(ggplot2)

df <- read.csv("train.csv", header = TRUE)
head(df)

##### Omit dictionary variables and long text
df = df[,-c(4,9,12,13,16,18,20,21,22)]

##### MISSINGG VAUES
df[df== ""] <- NA
na.cols <- which(colSums(is.na(df)) > 0)
na.cols <- sort(colSums(sapply(df[na.cols], is.na)), decreasing = TRUE)
na.cols

df = df %>% 
  mutate(
    belongs_to_collection_flag = ifelse(is.na(belongs_to_collection) == TRUE,1,0),
    hasHomePage = ifelse(is.na(homepage) == TRUE, 1, 0))

df = df[,-c(2,4)]
df = na.omit(df)

##### Date Cleaning
date.format <- as.Date(df$release_date, format="%m/%d/%Y")
year.fun = function(x){
  if(is.na(x)){
    return(paste0("2000"))
  }else if(x < 10){
    return(paste0("200",x))
  }else if(x >=10 & x <= 18){
    return(paste0("20", x))
  }else{
    return(paste0("19",x))
  }
}

df = df %>% 
  mutate(year = year(date.format),
         year = sapply(year, year.fun) %>% as.numeric(),
         month = month(date.format),
         weekday = as.numeric(as.factor(weekdays(date.format))))

##### Number Cleaning
describe(df)
for (i in 1:nrow(df)) {
  if(df[i,"budget"] > 1000 & df[i,"revenue"] < 100){
    df[i,"revenue"] = df[i,"revenue"] * 10^6
  }
}

##### Near zero variance
nzv = nearZeroVar(df)
df = df[, -nzv]

##DATA CLEANING END


#####Important Feature Analysis
# 1. Budget
par(cex = 0.7)
plot(df$budget,df$revenue, 
     pch = 21, lwd = 0.4, bg = "hotpink1",
     main = "Revenue vs. Budget",
     xlab = "Budget", ylab = "Revenue")
abline(lm(revenue~budget,data=df),col="blue",lwd=1.5)

# 2. Popularity
par(cex = 0.7)
plot(df$popularity,df$revenue, 
     pch = 21, lwd = 0.4, bg = "hotpink1",
     main = "Revenue vs. Popularity",
     xlab = "Popularity", ylab = "Revenue")
abline(lm(revenue~popularity,data=df),col="blue",lwd=1.5)


# 3. Runtime
par(cex = 0.7)
plot(df$runtime,df$revenue, 
     pch = 21, lwd = 0.4, bg = "hotpink1",
     main = "Revenue vs. Runtime",
     xlab = "Runtime", ylab = "Revenue")
abline(lm(revenue~runtime,data=df),col="blue",lwd=1.5)


# Revenue
par(cex = 0.65) 
hist(df$revenue,
     col  = "lightblue1",
     main = "Histogram of Revenue",
     xlab = "Revenue")


##### Modeling
model1 = lm(revenue~budget+popularity+runtime,data=df)
summary(model1) #Adjusted R-squared:  0.6141
