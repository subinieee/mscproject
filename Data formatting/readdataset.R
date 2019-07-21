library(dplyr)
library(readr)
library(openxlsx)



nrdata<-1
nrtestdata<-1
TV <-10
N<-100#_arr<-c(10,50,100)
p_number<-40#P<-c(20,40,60,80,100)


datasrc <-paste0("~/GitHub/mscproject/Data//EOT/N",N,"_P20-100_TV10(",nrdata,")")
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

data_test <-read.delim(paste0("~/GitHub/mscproject/Data/EOT/Test(",nrtestdata,")/sd",p_number,"_test/SD",p_number,".dat"),
                       skip=3, 
                       sep=(" "),
                       nrows=100, 
                       header = FALSE, 
                       stringsAsFactors = FALSE)
data_test <- data_test[,-1]

#column names

col_names <-list(1:ncol(data_train))
col_names <-paste0("","covariate",col_names[[1]],"")
colnames(data_train) <-col_names
colnames(data_train)[length(colnames(data_train))-1] <- "time"
colnames(data_train)[length(colnames(data_train))] <- "event"

col_names <-list(1:ncol(data_test))
col_names <-paste0("","A",col_names[[1]],"")
colnames(data_test) <-col_names
colnames(data_test)[length(colnames(data_test))-1] <- "time"
colnames(data_test)[length(colnames(data_test))] <- "event"



