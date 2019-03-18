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
names(df)

##### Omit dictionary variables and long text
df = df[,-c(9,11,12,18,20,21,22)]

##### MISSINGG VAUES
df[df== ""] <- NA
na.cols <- which(colSums(is.na(df)) > 0)
na.cols <- sort(colSums(sapply(df[na.cols], is.na)), decreasing = TRUE)
na.cols

df = df %>% 
  mutate(
    belongs_to_collection_flag = ifelse(is.na(belongs_to_collection) == TRUE,1,0),
    hasHomePage = ifelse(is.na(homepage) == TRUE, 1, 0))

df = subset(df, select=-c(belongs_to_collection,homepage))
df$runtime[is.na(df$runtime)] <- 0

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


##### JSON - production country
str_converter <- function(x, slasher = FALSE){
  string_split <- str_split(x, '"', simplify = TRUE)
  # Split the string in parts where " is found as an exception
  index1 <- (1:length(string_split)) %% 2 != 0
  # index1 is used to indicate where the single quote( ' ) has to be reversed
  index2 <- (1:length(string_split)) %% 2 == 0
  # index 2 is used to suppress unwanted exceptions in names
  string_split[index1] <- gsub("'", "\"", string_split[index1])
  string_split[index2] <- "0"
  string_all <- paste(string_split, sep = "\"", collapse = "")
  # repaste together the modified pieces with the suppressed corrupted names
  string_all <- gsub("None", '"None"', string_all)
  # Corrects the None without parenthesis
  if (slasher == TRUE){                      # This needs improvement!
    string_all <- gsub("\\\\\\D", "", string_all)}
  string_all
}

for(i in 1:nrow(df)) {
  row <- df[i,]
  if (is.na(row$production_countries) == FALSE){
      mydf <- fromJSON(str_converter(row$production_countries))
      for (n in mydf$iso_3166_1){
        df[i,n] <- 1}}
}

df = subset(df, select=-c(production_countries))
head(df)

##### JSON - genres
for(i in 1:nrow(df)) {
  row <- df[i,]
  if (is.na(row$genres) == FALSE){
    mydf <- fromJSON(str_converter(row$genres))
    for (n in mydf$name){
      df[i,n] <- 1}}
}

df = subset(df, select=-c(genres))
head(df)


##### JSON - spoken language
for(i in 1:nrow(df)) {
  row <- df[i,]
  if (is.na(row$spoken_languages) == FALSE){
    mydf <- fromJSON(str_converter(row$spoken_languages))
    for (n in mydf$iso_639_1){
      df[i,paste("Spoken_language",n,sep="_")] <- 1}}
}

df = subset(df, select=-c(spoken_languages))

df[is.na(df)] <- 0
head(df)

write.csv(df, file = "0311_cleaned_train.csv")

##DATA CLEANING END


#####Important Feature Analysis
# 1. Budget
par(mfrow=c(1,3))
plot(df$budget,df$revenue, 
     pch = 21, lwd = 0.4, bg = "hotpink1",
     main = "Revenue vs. Budget",
     xlab = "Budget", ylab = "Revenue")
abline(lm(revenue~budget,data=df),col="blue",lwd=1.5)

# 2. Popularity
plot(df$popularity,df$revenue, 
     pch = 21, lwd = 0.4, bg = "hotpink1",
     main = "Revenue vs. Popularity",
     xlab = "Popularity", ylab = "Revenue")
abline(lm(revenue~popularity,data=df),col="blue",lwd=1.5)


# 3. Runtime
plot(df$runtime,df$revenue, 
     pch = 21, lwd = 0.4, bg = "hotpink1",
     main = "Revenue vs. Runtime",
     xlab = "Runtime", ylab = "Revenue")
abline(lm(revenue~runtime,data=df),col="blue",lwd=1.5)


# Revenue
hist(df$revenue,
     col  = "lightblue1",
     main = "Histogram of Revenue",
     xlab = "Revenue")


##### Modeling
names(df)

model1 = lm(revenue~budget+popularity+runtime,data=df)
summary(model1) #Adjusted R-squared:  0.6141

model2 = lm(revenue~.-title-original_title-imdb_id-X...id,data=df)
summary(model2) #Adjusted R-squared:  0.7682
