
library(dplyr)
library(ggplot2)
library(ggRandomForests)
library(parallel)
library(caret)
library(readr)
library(openxlsx)
library(reshape2)
library(Amelia)

#data structuring

data_train <-read.csv("~/Desktop/patient_dat.txt")
# exclude unrelated columns
data_train <-data_train[,(1:which(names(data_train)=='FRET'))]
#separate pfs outcome and os outcome
outcome_pfs<-data_train[,c(60,62)]
outcome_os<-data_train[,c(61,63)]
data_train<-data_train[,-c(60:63,1)]

missmap(data_train)
colSums(is.na(data_train))
colSums(!is.na(data_train))
hist(outcome_os$osevent,las=2)
hist(outcome_os$osevent,las=2)
#combine outcome data of interest with train data
data_train_os<-(cbind(data_train,outcome_os))


colnames(data_train_os)[length(colnames(data_train_os))-1] <- "time"
data_train_os$time <-data_train_os$time/365
colnames(data_train_os)[length(colnames(data_train_os))] <- "event"
data_train_os$event <-as.numeric(data_train_os$event) 

censored_before<-which(data_train_os$time[which(data_train_os$event==0)]<median(data_train_os$time))
# Create the gg_survival object
gg_dta <- gg_survival(interval="time",
                      censor="event",
                      data=data_train_os,
                      conf.int=0.95)

# survival probability vs observation time
plot(gg_dta) +
  labs(y="survival Probability", 
       x="Observation Time(years)",
       color="Treatment", 
       fill="treatment") +
  theme(legend.position=c(0.2,0.2)) +
  coord_cartesian(y=c(0,1.01))


#cumulative hazard vs obs time
plot(gg_dta, type = "cum_haz") +
  labs(y = "Cumulative Hazard",
       x = "Observation Time (years)",
       color = "Treatment", fill = "Treatment") +
  theme(legend.position = c(0.2, 0.8)) + 
  coord_cartesian(ylim = c(-0.02, 1.22))




set.seed(1)


#train set train (default nodesize is 15 for survival family - the paper used 3)
rfsrc_train <- rfsrc(Surv(time,event) ~ .,
                      ntree = 750,
                     nodesize = 8,
                     mtry=13,
                      data=data_train_os,
                      nsplit=10, 
                      na.action="na.impute",
                      tree.err=TRUE, 
                      importance=TRUE,
                      proximity = TRUE)


print(rfsrc_train)


#Generalisation error
gg_error1<-gg_error(rfsrc_train)
plot(na.omit(gg_error1))  




#strCols should be defined "TRUE" for death(event), "FALSE" for censored
strCol <- c("TRUE" = "red",
            "FALSE"="blue")

ggRFsrc <-plot(gg_rfsrc(rfsrc_train),alpha=0.2)+
  theme(legend.position="none")+
  scale_color_manual(values = strCol)+
  labs(y="survival Probability",
       x="Time(years)")+
  coord_cartesian(ylim=c(-0.01,1.01))
show(ggRFsrc)


# variable importance(lbls=st.labs needs to be defined)
vimp<-gg_vimp(rfsrc_train)
plot(gg_vimp(rfsrc_train))+
  theme(legend.position=c(p=0.8,0.2))+
  labs(fill="VIMP > 0")

var.select(rfsrc_train)
#test set predictions


#Prediction Accuracy 
#find cut-off time(median survival time)
t<- median(data_train_os$time)
m <- which.min(abs(rfsrc_train$time.interest - t))
t<- rfsrc_train$time.interest[m]  
#
# Get the ground truth as a vector of who did survive beyond t
truth <- data_train_os$time >t
if(length(censored_before)!=0){
  truth <- data_train_os$time[-censored_before] > t
}
#
# Get the prediction of who rf thinks will survive > t
pred_train <- rfsrc_train$survival.oob[,m] >0.5 #rfsrc_train$survival[,m] > 0.5
if(length(censored_before)!=0){
  pred_train <-pred_train[-censored_before]
}
# Confusion matrix
cm_train <- matrix(c(length(which(truth=="FALSE" & pred_train=="FALSE")),length(which(truth=="FALSE" & pred_train=="TRUE")),
                     length(which(truth=="TRUE" & pred_train=="FALSE")),length(which(truth=="TRUE" & pred_train=="TRUE"))),
                   ncol=2,byrow=TRUE)
colnames(cm_train) <- c("FALSE","TRUE")
rownames(cm_train) <- c("FALSE","TRUE")
cm_train <- as.table(cm_train)


# Accuracy  
accuracy_train <- (cm_train[1,1] + cm_train[2,2])/sum(cm_train)

#collect data in an array(replace 'a'th number in the array after each loop)

#reset the "a" value to keep it =<3
#collect average accuary values of the model at different P values
#create dataframes and combine with old data
tune<-tune.rfsrc(Surv(time, event) ~ ., data=data_train_os, na.action="na.impute", trace = TRUE, doBest = TRUE)$err

