install.packages('caret', dependencies = TRUE)
install.packages('dplyr')
install.packages('e1071')
install.packages('mice')
install.packages('gdata')
install.packages('psych')
install.packages('DMwR',dependencies = TRUE)
library(caret)
library(readr)
library(e1071)
library(ggplot2)
library(dplyr)
library(DMwR)
library(mice)
library(gdata)
library('psych')

rawData <- read.table("C:/Springboard/DS Fundamentals/R Code/Capstone/data/adult.data.txt", header = FALSE, sep = ",")

names(rawData) <- c("age", "workclass", "fnlwwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income50")
View(rawData)

factor_idx = which(unlist(lapply(rawData, class)) == "factor")
factor_idx
factor_idx_depvar <- (factor_idx[-9])  #remove column income50
factor_idx_depvar

#Identify releative column number for factors to be used to replace "?" with "NA"
for (i in 1:NROW(factor_idx_depvar)) {
  naidx <- which(as.character(rawData[, factor_idx_depvar[i]]) == " ?")
  rawData[naidx, factor_idx_depvar[i]] <- NA
}

?factor
#rawData <- factor(rawData)

select(rawData) %>% group_by(rawData$occupation) %>% summarise(count = n())

occupation_plot <- ggplot(rawData, aes(occupation), stat_count()) + geom_bar()
occupation_plot

#Required to replace ? level and replace with NA; other wise imputaion substitutes "?" from original data
rawData <- drop.levels(rawData)
levels(rawData$workclass)
levels(rawData$occupation)
levels(rawData$native_country)

#replace NA values with K nearest neighbor
?knnImputation
rawDataClean <- knnImputation(rawData, 4)

select(rawData) %>% group_by(rawDataClean$workclass) %>% summarise(count = n()) 
select(rawData) %>% group_by(rawDataClean$occupation) %>% summarise(count = n())
select(rawData) %>% group_by(rawDataClean$native_country) %>% summarise(count = n())
levels(rawDataClean$occupation)

View(rawDataClean)

preProcModel = preProcess(rawDataClean, method = c("center", "scale", "zv", "nzv"))
processedData = predict(preProcModel, rawDataClean)
str(processedData)
View(processedData)
summary(processedData)

#Create dummy codes for factors, this is required for svm modeling
processedData$workclass <- dummy.code(processedData$workclass)
processedData$education <- dummy.code(processedData$education)
processedData$marital_status <- dummy.code(processedData$marital_status)
processedData$occupation <- dummy.code(processedData$occupation)
processedData$relationship <- dummy.code(processedData$relationship)
processedData$race <- dummy.code(processedData$race)
processedData$sex <- dummy.code(processedData$sex)
processedData$native_country <- dummy.code(processedData$native_country)

trainRows = sample(seq(1,nrow(processedData)),0.75*nrow(processedData))
length(trainRows)

trainData = processedData[trainRows,]
dim(trainData)
str(trainData)
View(trainData)
nrow(trainData)

#testRows = setdiff(seq(1,nrow(inputData), trainRows)
testRows = setdiff(seq(1, nrow(processedData)), trainRows)

testData = processedData[testRows,]
dim(testData)
str(testData)

#educational and initial purposes run on subset of trainData
#mysample <- trainData[sample(1:nrow(trainData), 5000, replace=FALSE),]
#nrow(mysample)
#View(mysample)

trainData <- trainData[sample(1:nrow(trainData), 5000, replace=FALSE),]
nrow(trainData)
View(trainData)

?svm
initialModel = svm(x=trainData[,1:(ncol(trainData)-1)],y=trainData[,ncol(trainData)])
summary(initialModel)
#predict(initialModel,trainData[,1:(ncol(trainData)-1)])
initialModelPredict <- predict(initialModel,trainData[,1:(ncol(trainData)-1)])
str(initialModelPredict)
NROW(initialModelPredict)
initialModel

svm.table <- table(initialModelPredict, trainData$income50)
classAgreement(svm.table)
confusionMatrix(svm.table)

#obsolete code
#tunedInitialModel <- tune.svm(income50~., data = trainData, gamma = 10^(-1:2), cost= 10^(1:2))
#tunedInitialModelPredict <- predict(tunedInitialModel,trainData[,1:(ncol(trainData)-1)])
#summary(tunedInitialModel)
#print(tunedInitialModel)

#mvs
svm.table <- table(initialModelPredict, trainData$income50)
classAgreement(svm.table)
confusionMatrix(svm.table)
#mvs

#tunedInitialModel2 <- tune(svm, train.x = trainData[,-13], train.y=trainData$income50, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
#ranges=list(cost=exp(c(-1,0,3,5,7,9)), gamma=exp(c(-1,0,3,5,7,9))))
#tunedInitialModel2 <- tune.svm(income50~., data=trainData, kernel="radial", ranges=list(cost=exp(c(-1,0,3,5,7,9)), gamma=exp(c(-1,0,3,5,7,9))))

tunedInitialModel2 <- tune(svm, train.x = trainData[,-13], train.y=trainData$income50, kernel="radial", ranges=list(cost=exp(c(-1,0,3,5,7,9)), gamma=exp(c(-1,0,3,5,7,9))))
summary(tunedInitialModel2)
print(tunedInitialModel2)
initialModelPredict2 <- predict(tunedInitialModel2,trainData[,1:(ncol(trainData)-1)])
svm.table2 <- table(tunedInitialModelPredict2, trainData$income50)
classAgreement(tunedInitialModel2)
confusionMatrix(svm.table2)

#<- (income50~., data = trainData, gamma = tuned$best.parameters$gamma, cost = tuned$best.parameters$cost)
summary(tunedInitialModel)

head(initialModelPredict)

rawData$income50[23403]


##########################################################################################################

################################################ Misc Code ################################################
sum(is.na(trainData$age) == TRUE)
sum(is.na(trainData$workclass) == TRUE)
sum(is.na(trainData$fnlwwgt) == TRUE)
sum(is.na(trainData$education) == TRUE)
sum(is.na(trainData$education_num) == TRUE)
sum(is.na(trainData$marital_status) == TRUE)
sum(is.na(trainData$occupation) == TRUE)
sum(is.na(trainData$relationship) == TRUE)
sum(is.na(trainData$race) == TRUE)
sum(is.na(trainData$sex) == TRUE)
sum(is.na(trainData$hours_per_week) == TRUE)
sum(is.na(trainData$native_country) == TRUE)

levels(trainData$income50)
levels(trainData$age)
levels(trainData$workclass)
levels(trainData$fnlwwgt)
levels(trainData$education)
levels(trainData$education_num) 
levels(trainData$marital_status)
levels(trainData$occupation)
levels(trainData$relationship)
levels(trainData$race)
levels(trainData$sex)
levels(trainData$hours_per_week)
levels(trainData$native_country)

age <- select(trainData) %>% group_by(trainData$age) %>% summarise(count = n()) 
View(age)
workclass <- select(trainData) %>% group_by(trainData$workclass) %>% summarise(count = n()) 
View(workclass)
fnlwwght <- select(trainData) %>% group_by(trainData$fnlwwgt) %>% summarise(count = n()) 
View(fnlwwght)
select(trainData) %>% group_by(trainData$occupation) %>% summarise(count = n())
native_country <- select(trainData) %>% group_by(trainData$native_country) %>% summarise(count = n())
View(native_country)
education <- select(trainData) %>% group_by(trainData$education) %>% summarise(count = n())
View(education)
education_num <- select(trainData) %>% group_by(trainData$education_num) %>% summarise(count = n())
View(education_num)
marital_status <- select(trainData) %>% group_by(trainData$marital_status) %>% summarise(count = n())
View(marital_status)
occupation <- select(trainData) %>% group_by(trainData$occupation) %>% summarise(count = n())
View(occupation)
relationship <- select(trainData) %>% group_by(trainData$relationship) %>% summarise(count = n())
View(relationship)
race <- select(trainData) %>% group_by(trainData$race) %>% summarise(count = n())
View(race)
sex <- select(trainData) %>% group_by(trainData$sex) %>% summarise(count = n())
View(sex)
hours_per_week <- select(trainData) %>% group_by(trainData$hours_per_week) %>% summarise(count = n())
View(hours_per_week)
native_country <- select(trainData) %>% group_by(trainData$native_country) %>% summarise(count = n())
View(native_country)
sum(native_country$count)
nrow(trainData)

age_plot <- ggplot(trainData, aes(age), stat_count()) + geom_bar()
age_plot
workclass_plot <- ggplot(trainData, aes(workclass), stat_count()) + geom_bar()
workclass_plot
fnlwwgt_plot <- ggplot(trainData, aes(fnlwwgt), stat_count()) + geom_bar()
fnlwwgt_plot
education_plot <- ggplot(trainData, aes(education), stat_count()) + geom_bar()
education_plot
education_num_plot <- ggplot(trainData, aes(education_num), stat_count()) + geom_bar()
education_num_plot
marital_status_plot <- ggplot(trainData, aes(marital_status), stat_count()) + geom_bar()
marital_status_plot
occupation_plot <- ggplot(trainData, aes(occupation), stat_count()) + geom_bar()
occupation_plot
relationship_plot <- ggplot(trainData, aes(relationship), stat_count()) + geom_bar()
relationship_plot
race_plot <- ggplot(trainData, aes(race), stat_count()) + geom_bar()
race_plot
sex_plot <- ggplot(trainData, aes(sex), stat_count()) + geom_bar()
sex_plot
#capital_gain_plot <- ggplot(trainData, aes(capital_gain), stat_count()) + geom_bar()
#capital_gain_plot
hours_per_week_plot <- ggplot(trainData, aes(hours_per_week), stat_count()) + geom_bar()
hours_per_week_plot
native_country_plot <- ggplot(trainData, aes(native_country), stat_count()) + geom_bar()
native_country_plot
################################## Misc Code ############################################################
#install.packages('imputation')
#library(imputation)
#setwd("C:/Springboard/DS Fundamentals/Data Wrangling/RCode")
#headerData <- read.table("C:/Springboard/DS Fundamentals/R Code/Capstone/data/manualHeader.txt", header = FALSE, sep = " ")
str(rawData)

lapply(rawData, class)  #data type

lapply(rawData, summary) #counts per column value

lapply(rawData$age, hist) #? hist(rawdata$age)

pairs(rawData)  #R hangs

levels(rawData$income50)

occupation_plot <- ggplot(rawData, aes(occupation), stat_count()) + geom_bar()
occupation_plot

#rawData_Impute <- knnImpute()
#rawDataClean <- preProcModel = preProcess(rawData, method = c("center", "scale", "knnImpute", "zv", "nzv")))
#naidx <- which(as.character(rawData$occupation) == " ?")
#naidx

levels(rawData$occupation)
nrow(rawData)

select(rawDataClean) %>% group_by(rawData$occupation) %>% summarise(count = n())
str(rawData_clean)

occupationCleanPlot <- ggplot(rawDataClean, aes(occupation), stat_count()) + geom_bar()
occupationCleanPlot

select(rawData) %>% group_by(rawDataClean$occupation) %>% summarise(count = n())
select(rawDataClean) %>% group_by(rawDataClean$occupation) %>% summarise(count = n())
levels(rawData$occupation)
levels(rawDataClean$occupation)

workclass_plot <- ggplot(rawDataClean, aes(workclass), stat_count()) + geom_bar()
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

levels(rawDataClean$workclass) 
levels(rawDataClean$education) 
levels(rawDataClean$marital_status) 
levels(rawDataClean$occupation) 
levels(rawDataClean$relationship) 
levels(rawDataClean$race) 
levels(rawDataClean$sex) 
levels(rawDataClean$native_country)

rm(workclass_plot, education_plot, marital_status_plot, occupation_plot, relationship_plot, race_plot, sex_plot, native_country_plot)

