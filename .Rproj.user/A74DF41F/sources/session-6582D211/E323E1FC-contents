#clearing the environment
rm(list = ls())

train = read.csv("titanic_train.csv")
test = read.csv("titanic_test.csv")
library(ggplot2)
library(scales)
library(dplyr)
library(randomForest)
library(caTools)
str(train)
colnames(train)
#logistic model
lm_1 = glm(Survived ~ Pclass+Age+Fare,data =train,family = binomial)
summary(lm_1)
lm_2 = glm(Survived ~ Pclass+Age,data =train,family = binomial)
summary(lm_2)

predictions = predict(lm_2, type="response", newdata=test)
test$predictions= predictions
#creating data and adding predictions
titanicpredDF = data.frame(test,predictions)
titanicpredDF$titanicprediction = titanicpredDF$predictions>0.5
predictions = predict(lm_2, type="response", newdata=train)
train$predictions= predictions
#creating data and adding predictions
titanicpredDF = data.frame(train,predictions)
titanicpredDF$titanicprediction = titanicpredDF$predictions>0.5

#confusion matrix
# > 0.5, TRUE and <0.5, FALSE
cf = table(titanicpredDF$Survived,titanicpredDF$titanicprediction)
# to check for data frame
cf = as.data.frame.matrix(table(titanicpredDF$Survived,titanicpredDF$titanicprediction))

View(cf)# view table of Cf
accuracy = (347+150)/(347+150+77+140)# evaluating the performance of the model 
truepositive = (347)/(347+150+77+140)
truenegative = (150)/(347+150+77+140)
Falsepositive = (140)/(347+150+77+140) # also called type1 error
Falsenegative = (77)/(347+150+77+140) # also called type2 error