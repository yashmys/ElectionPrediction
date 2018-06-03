install.packages("mice")
install.packages("contrib.url")
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

Train <- subset(polling, polling$Year == 2004 | polling$Year == 2008)
Test <- subset(polling,polling$Year == 2012)
dim(Train)

# get the baseline accuracy
table(Train$Republican,sign(Train$Rasmussen))

#get the corelation among the independent variables 
cor(Train[c("Rasmussen","SurveyUSA","DiffCount","PropR","Republican")])

mod1 <- glm(Train$Republican ~ Train$PropR,data = Train,family = binomial)
summary(mod1)

pred1 <- predict(mod1,type="response")
summary(pred1)
pred1

table(Train$Republican,pred1 > 0.5)

mod2 <-  glm(Republican ~ SurveyUSA + DiffCount,data = Train,family = binomial)
summary(mod2)
pred2 <- predict(mod2,type="response")
table(Train$Republican,pred2 > 0.5)


# baseline model for test sample
dim(Test)
table(Test$Republican,sign(Test$Rasmussen))
TestPrediction <- predict(mod2,newdata = Test,type = "response")
table(Test$Republican,TestPrediction > 0.5)
