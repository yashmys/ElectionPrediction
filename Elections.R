install.packages("mice")
library('tidyverse')
library('caTools')
library('ROCR')
library('mice')

polling <- read.csv("PollingData.csv")
dim(polling)
names(polling)
table(polling$Year)
summary(polling)

req_col <- subset(polling,select = c ( "Rasmussen" , "SurveyUSA" , "DiffCount" , "PropR" ))
dim(req_col)
head(req_col)

set.seed(300)
imputed <-  complete(mice(req_col))
summary(imputed)

polling$Rasmussen <-  imputed$Rasmussen
polling$SurveyUSA <-  imputed$SurveyUSA
summary(polling)