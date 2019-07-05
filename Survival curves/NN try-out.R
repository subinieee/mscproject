library(neuralnet)
library(randomForestSRC)
library(dplyr)
library(ggplot2)
library(ggRandomForests)
library(parallel)
library(plot3D)
library(dummies)
data(pbc)
data<-na.omit(pbc)
summary(pbc)
pbc.trial <- data[1:130,]
pbc.test <- data[-c(1:130),]
nn=neuralnet(days ~.,data=pbc.trial, hidden=5,act.fct = "logistic",
             linear.output = FALSE)
plot(nn,rep = "best")

model_train<-OneR(pbc.trial, verbose=TRUE)
prediction <- predict(model_train,pbc.test)
eval_model(prediction, pbc.test)
