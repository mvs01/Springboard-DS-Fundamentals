install.packages('caret', dependencies = TRUE)
install.packages('dplyr')
install.packages('e1071')
install.packages('imputation')
install.packages('DMwR',dependencies = TRUE)
library(caret)
library(readr)
library(e1071)
library(ggplot2)
library(dplyr)
library(DMwR)

#setwd("C:/Springboard/DS Fundamentals/Data Wrangling/RCode")

rawData <- read.table("C:/Springboard/DS Fundamentals/R Code/Capstone/data/adult.data.txt", header = FALSE, sep = ",")

#headerData <- read.table("C:/Springboard/DS Fundamentals/R Code/Capstone/data/manualHeader.txt", header = FALSE, sep = " ")

names(rawData) <- c("age", "workclass", "fnlwwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income50")
View(rawData)

str(rawData)

lapply(rawData, class)  #data type

lapply(rawData, summary) #counts per column value

lapply(rawData$age, hist) #? hist(rawdata$age)

pairs(rawData)  #R hangs

#Identify releative column number for factors to be used to replace "?" with "NA"
factor_idx = which(unlist(lapply(rawData, class)) == "factor")
factor_idx
factor_idx_depvar <- (factor_idx[-9])  #remove column income50
factor_idx_depvar

#naidx <- which(as.character(rawData$occupation) == " ?")
#naidx

for (i in 1:NROW(factor_idx_depvar)) {
  naidx <- which(as.character(rawData[, factor_idx_depvar[i]]) == " ?")
  rawData[naidx, factor_idx_depvar[i]] <- "NA"
}

preProcModel = preProcess(rawData, method = c("center", "scale", "zv", "nzv"))

#select(rawData, everything()) %>% if (rawData$occupation == "?") {rawData$occupation <- "NA"}

#for (i in 1:nrow(rawData)) {
#  print(i)
#  for (j in 1:NROW(factor_idx_depvar)) {
#     print(j)
#     factor_idx_depvar[j]
#     if (rawData[i, factor_idx_depvar[j]] == "?") {
#       rawData[i, factor_idx_depvar[j]] <- "NA"
#     } 
#  }
#  if (i >100) {break}
#}

workclass_plot <- ggplot(rawData, aes(workclass), stat_count()) + geom_bar()
workclass_plot

education_plot <- ggplot(rawData, aes(education), stat_count()) + geom_bar()
education_plot

marital_status_plot <- ggplot(rawData, aes(marital_status), stat_count()) + geom_bar()
marital_status_plot

occupation_plot <- ggplot(rawData, aes(occupation), stat_count()) + geom_bar()
occupation_plot

relationship_plot <- ggplot(rawData, aes(relationship), stat_count()) + geom_bar()
relationship_plot

race_plot <- ggplot(rawData, aes(race), stat_count()) + geom_bar()
race_plot

sex_plot <- ggplot(rawData, aes(sex), stat_count()) + geom_bar()
sex_plot

native_country_plot <- ggplot(rawData, aes(native_country), stat_count()) + geom_bar()
native_country_plot
levels(rawData$native_country)

age_plot <- ggplot(rawData, aes(income50, age), stat_count()) + geom_bar()
age_plot


rm(workclass_plot, education_plot, marital_status_plot, occupation_plot, relationship_plot, race_plot, sex_plot, native_country_plot)

#replace NA values with K nearest neighbor
rawData_clean <- knnImputation(rawData, 4)

processedData = predict(preProcModel, rawData)
summary(processedData)
#corr(income50 ~ .)

trainRows = sample(seq(1,nrow(rawData)),0.75*nrow(rawData))
length(trainRows)

trainData = rawData[trainRows,]
dim(trainData)

#testRows = setdiff(seq(1,nrow(inputData), trainRows)
testRows = setdiff(seq(1, nrow(rawData)), trainRows)
str(testRows)
View(testRows)

testData = rawData[testRows,]
dim(testData)

ncol(trainData)
?svm
initialModel = svm(x=trainData[,1:(ncol(trainData)-1)],y=trainData[,ncol(trainData)])










