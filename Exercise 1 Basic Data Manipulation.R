setwd("C:/Springboard/DS Fundamentals/Data Wrangling")

library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(tibble)
library(caret)

library(devtools)
devtools::install_github("mvs01/Springboard-DS-Fundamentals")

#0
refine_original <- read_csv("C:/Springboard/DS Fundamentals/Data Wrangling/refine _original.csv")

#1
refine_original$company <- tolower(refine_original$company)

phillips <- c(grep("*ips$", refine_original$company))
akzo <- c(grep("^ak*", refine_original$company))
unilever <- c(grep("*ver$", refine_original$company))

for(i in 1:length(akzo)) {
  refine_original[akzo[i],]$company <- "akzo"
}

for(i in 1:length(phillips)) {
  refine_original[phillips[i],]$company <- "phillips"
}

for(i in 1:length(unilever)) {
 refine_original[unilever[i],]$company <- "unilever"
}

#2
refine_original <-separate(refine_original, `Product code / number`, c("product_code", "product_number"), sep = "-", remove = FALSE)

#3
product_category = as.character(c(1:nrow(refine_original)))

product_category_df = data.frame(product_category)
product_category_df <- as.character(product_category_df)

refine_original <- cbind(refine_original, product_category)
refine_original$product_category <- as.character(refine_original$product_category)

for(i in 1:nrow(refine_original)) {
  if (refine_original[i,]$product_code == "p") {
    refine_original[i,]$product_category = "Smartphone" 
  }
  else 
  if  (refine_original[i,]$product_code == "v") {
    refine_original[i,]$product_category = "TV" 
  }
  else 
  if (refine_original[i,]$product_code == "x") {
    refine_original[i,]$product_category = "Laptop" 
  }
  else 
  if  (refine_original[i,]$product_code == "q") {
    refine_original[i,]$product_category = "Tablet" 
    }
}

#4
refine_original <-unite(refine_original, full_address, address, city, country, sep = ",", remove = FALSE)

#5
refine_original$company_phillips <- NA
levels(refine_original$company_phillips) <- c(0,1)

refine_original$company_akzo <- NA
levels(refine_original$company_akzo) <- c(0,1)

refine_original$company_van_houten <- NA
levels(refine_original$company_van_houten) <- c(0,1)

refine_original$company_unilever <- NA
levels(refine_original$company_unilever) <- c(0,1)

#6
write.csv(refine_original, file="refine_clean.csv", row.names=T)





##################Experiment Code######################

View(refine_original)

warnings()

?regex

for(i in 1:nrow(refine_original)) {
  print(refine_original[i,]$product_code) 
}


rm(factor_binary_vectoror)

binary_vector <- c(0, 1)
factor_binary_vector <- factor(binary_vector)
levels(factor_binary_vector) <- c(0, 1)
factor_binary_vector

str(binary_factor)
class(binary_factor)

binary_factor <- 3
binary_factor

refine_original <- add_column(refine_original, product_category = 1:refine_original_rows)

refine_original <- as.character(refine_original$product_category)





View(refine_original)
str(refine_original)

rm(refine_original_rows)



warnings()

rm(refine_original)

print(phillips[5])

View(product_category_df)

temp <- strsplit(refine_original$`Product code / number`, "-")
class(temp)

for(i in 1:nrow(refine_original)){
  print(i)
  print(refine_original[i,]$company)
  temp_ref_orig <- strsplit(refine_original$`Product code / number`, "-")
  #print(temp_ref_orig[[i]][1])
  #print(temp_ref_orig[[i]][2])
  prod_cd <- as.character(temp_ref_orig[[i]][1])
  prod_nbr <- as.numeric(temp_ref_orig[[i]][2])
  print(prod_cd)
  print(prod_nbr)
  refine_original[i]$product_code <- prod_cd
  refine_original[i]$product_number <- prod_nbr
  print(" ")
}

for(i in 1:nrow(refine_original)){
  print(i)
  print(temp[[i]][1])
  print(temp[[i]][2])
  print(refine_original[i,]$company)
  #refine_original[i]$product_code <- temp[[i]][1]
  #refine_original[i]$product_number <- temp[[i]][2]
}

?lapply

temp
refine_original
