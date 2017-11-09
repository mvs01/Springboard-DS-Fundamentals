setwd("C:/Springboard/DS Fundamentals/Data Wrangling")

library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(tibble)
library(caret)

#0
titanic_original <- read_csv("C:/Springboard/DS Fundamentals/Data Wrangling/titanic_original.csv")

#1
distinct(titanic_original, embarked)

for (i in 1:nrow(titanic_original)) {
    if (is.na(titanic_original[i,]$pclass) == TRUE) {
    next
  }  
  else if (is.na(titanic_original[i,]$embarked) == TRUE) {
    titanic_original[i,]$embarked <- "S"
  } 
}

#2
titanic_age_mean <- mean(titanic_original$age, na.rm = TRUE)

for (i in 1:nrow(titanic_original)) {
  if (is.na(titanic_original[i,]$age) == TRUE) {
    titanic_original[i,]$age <- titanic_age_mean
  }
}

#3
for (i in 1:nrow(titanic_original)) {
  if (is.na(titanic_original[i,]$pclass) == TRUE) {
    next
  }  
  else if (is.na(titanic_original[i,]$boat) == TRUE) {
    titanic_original[i,]$boat <- "None"
  } 
}

#4
has_cabin_number <- data.frame(c(1:nrow(titanic_original)))
levels(has_cabin_number) <- c(0,1)
names(has_cabin_number) <- c("has_cabin_number")


for (i in 1:nrow(titanic_original)) {
  if (is.na(titanic_original[i,]$cabin) == TRUE) {
   has_cabin_number[i,] <- 1
  }  
  else {
   has_cabin_number[i,] <- 0
  } 
}

titanic_original <-cbind(titanic_original, has_cabin_number)

#5
write.csv(titanic_original, file="titanic_clean.csv", row.names=T)

#++++++++++++++++++++++dummy code++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
View(titanic_original)
str(titanic_original)

rm(titanic_original)
rm(has_cabin_number)

titanic_original[1310]

for (i in 1:nrow(titanic_original)) {
  if (i > 210) {
    break
  }  
  else {
     print(i)
     print(titanic_original[i,]$embarked)
  } 
}

for (i in 1:nrow(titanic_original)) {
  if (is.na(titanic_original[i,]$boat) == TRUE) {
    print(i)
  }
}




