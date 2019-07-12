library(openxlsx)
library(randomForestSRC)
library(dplyr)
library(ggplot2)
library(ggRandomForests)
library(parallel)
library(caret)
library(readr)
library(openxlsx)
library(reshape2)
library(Amelia)
library(plyr)

#data structuring
data2<-read.csv("~/Desktop/COIN/data2.txt", stringsAsFactors = TRUE)
data_train <-read.csv("~/Desktop/COIN/patient_dat.txt", stringsAsFactors = TRUE)
# exclude unrelated columns
data_train <-data_train[,(1:which(names(data_train)=='FRET'))]
data2 <- data2[,-1]
#separate pfs outcome and os outcome
outcome_pfs<-data_train[,c(60,62)]
outcome_os<-data_train[,c(61,63)]
data_train<-data_train[,-c(60:63,1)]

summary(data2)
missmap(data2)
colSums(is.na(data_train))/1630
data2<-data2[-which(colSums(is.na(data2))/1630>0.20)]
missmap(data2)
data_train_pfs<-(cbind(data2,outcome_pfs))

colnames(data_train_pfs)[length(colnames(data_train_pfs))-1] <- "time"
data_train_pfs$time <-data_train_pfs$time/365
colnames(data_train_pfs)[length(colnames(data_train_pfs))] <- "event"
data_train_pfs$event <-as.numeric(data_train_pfs$event) 
censored_before<-which(data_train_pfs$time[which(data_train_pfs$event==0)]<median(data_train_pfs$time))

df<-cbind("no.covariates"=0,"accuracy_train"=0, "accuracy_OOB"=0, "err_rate"=0)
active_covariates<-list(0)

for(i in c(1:length(colnames(data2)))){
  
  set.seed(1)
  #data_train_pfs<-data_train_pfs %>% mutate_if(sapply(data_train_pfs, is.character), as.factor)
  #sapply(data_train_pfs, class)
  #train set train (default nodesize is 15 for survival family - the paper used 3)
  rfsrc_train <- rfsrc(Surv(time,event) ~ .,
                       ntree = 1000,
                       #nodesize = 6,
                       #mtry=32,
                       data=data_train_pfs,
                       nsplit=10, 
                       na.action="na.impute",
                       tree.err=TRUE, 
                       importance=TRUE,
                       proximity = TRUE)
  
  
  print(rfsrc_train)
  
  # variable importance(lbls=st.labs needs to be defined)
  vimp<-gg_vimp(rfsrc_train)
  print(vimp)
  #Prediction Accuracy 
  #find cut-off time(median survival time)
  t<- median(data_train_pfs$time)
  m <- which.min(abs(rfsrc_train$time.interest - t))
  t<- rfsrc_train$time.interest[m]  
  #
  # Get the ground truth as a vector of who did survive beyond t
  truth <- data_train_pfs$time >t
  if(length(censored_before)!=0){
    truth <- data_train_pfs$time[-censored_before] > t
  }
  
  #
  # Get the prediction of who rf thinks will survive > t
  pred_OOB <- rfsrc_train$survival.oob[,m] >0.5 
  pred_train <- rfsrc_train$survival[,m] > 0.5
  if(length(censored_before)!=0){
    pred_train <-pred_train[-censored_before]
    pred_OOB <-pred_OOB[-censored_before]
  }
  
  # Confusion matrix
  cm_train <- matrix(c(length(which(truth=="FALSE" & pred_train=="FALSE")),length(which(truth=="FALSE" & pred_train=="TRUE")),
                       length(which(truth=="TRUE" & pred_train=="FALSE")),length(which(truth=="TRUE" & pred_train=="TRUE"))),
                     ncol=2,byrow=TRUE)
  colnames(cm_train) <- c("FALSE","TRUE")
  rownames(cm_train) <- c("FALSE","TRUE")
  cm_train <- as.table(cm_train)
  
  cm_OOB <- matrix(c(length(which(truth=="FALSE" & pred_OOB=="FALSE")),length(which(truth=="FALSE" & pred_OOB=="TRUE")),
                       length(which(truth=="TRUE" & pred_OOB=="FALSE")),length(which(truth=="TRUE" & pred_OOB=="TRUE"))),
                     ncol=2,byrow=TRUE)
  colnames(cm_OOB) <- c("FALSE","TRUE")
  rownames(cm_OOB) <- c("FALSE","TRUE")
  cm_OOB <- as.table(cm_OOB)
  
  # Accuracy  
  accuracy_train <- (cm_train[1,1] + cm_train[2,2])/sum(cm_train)
  accuracy_OOB <- (cm_OOB[1,1] + cm_OOB[2,2])/sum(cm_OOB)
  #create dataframes and combine with old data
  active_covariates <-c(active_covariates,list(vimp$vars))
  df<-rbind(df,cbind("no.covariates"=length(colnames(data_train_pfs))-2,accuracy_train,accuracy_OOB, "C-Index"=1-rfsrc_train$err.rate[1000]))
  data_train_pfs<-data_train_pfs[-which(names(data_train_pfs)==vimp$vars[length(vimp$vars)])]
  
}


df<-as.data.frame(df)
write.xlsx(active_covariates,"~/Desktop/active_covariates.xlsx")
write.xlsx(df, "~/Desktop/learning_curve.xlsx")
df<-df[-1,]
active_covariates<-active_covariates[-1]
aa<-paste0(active_covariates)
colnames(active_covariates)
colnames(df)[4]<-"C-Index"
#restructure and aggregate data
Vis_df <- melt(df, id.vars = c('no.covariates'), measure.vars = c("accuracy_train","accuracy_OOB","C-Index"))

#best fitted line with 95% confidence interval in log scale.
ggplot(Vis_df, aes(x = no.covariates, y = value, color = variable)) +
  geom_point(aes(shape=variable),size = 2, alpha=0.3) +
  labs(y = "prediction performance", x = "no.covariates")+
  geom_smooth(method = "auto", alpha = .15, aes(fill = variable,linetype= variable),size = 0.8)+
  theme_bw()+
  theme(plot.title = element_text(size=14, face="bold.italic"),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=11,face = "bold"),
        axis.text.y = element_text(size=11,face="bold"),
        legend.text=element_text(size=11),
        legend.title = element_blank())
