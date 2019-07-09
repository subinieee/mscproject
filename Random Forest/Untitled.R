library(randomForestSRC)
library(dplyr)
library(ggplot2)
library(ggRandomForests)
library(parallel)
library(caret)
library(readr)
library(openxlsx)
library(reshape2)



nrdata<-3
nrtestdata<-1
TV <-10
N<-100#_arr<-c(10,50,100)
p_number<-20#P<-c(20,40,60,80,100)

datasrc <-paste0("~/GitHub/mscproject/Data/N",N,"_P20-100_TV10(",nrdata,")")
#Function for repeating RF with different datasets on a loop.

# Data cleaning (read .dat file and remove index column)
set.seed(1)
data_train <-read.delim(paste0("",datasrc,"/sd",p_number,"/SD",p_number,".dat"), 
                        skip=3, 
                        sep=(" "),
                        nrows=N, 
                        header = FALSE, 
                        stringsAsFactors = FALSE)
data_train <- data_train[,-1]

data_test <-read.delim(paste0("~/GitHub/mscproject/Data/Test(",nrtestdata,")/sd",p_number,"_test/SD",p_number,".dat"),
                       skip=3, 
                       sep=(" "),
                       nrows=100, 
                       header = FALSE, 
                       stringsAsFactors = FALSE)
data_test <- data_test[,-1]

#column names

col_names <-list(1:ncol(data_train))
col_names <-paste0("","A",col_names[[1]],"")
colnames(data_train) <-col_names
colnames(data_train)[length(colnames(data_train))-1] <- "time"
colnames(data_train)[length(colnames(data_train))] <- "event"
data_train$event <-as.numeric(data_train$event)
censored_before<-which(data_train$time[which(data_train$event==0)]<median(data_train$time))
nrevent_n <-rbind(nrevent_n,cbind("N"=N, "nr.event"=length(which(data_train$event=="1")), "E/N"=length(which(data_train$event=="1"))/N))

#Vis_df <- melt(nrevent_n, id.vars = c('N'), measure.vars = c('E/N'))
#ggplot(Vis_df, aes(x = N, y = value, color = variable) ) +
#  geom_point() +
#  geom_smooth(method = "lm", alpha = .15, aes(fill = variable))+ theme_bw()
  

