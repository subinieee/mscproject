library(randomForestSRC)
library(dplyr)
library(ggplot2)
library(ggRandomForests)
library(parallel)
library(caret)
library(readr)
library(openxlsx)



nrdata<-1
nrtestdata<-1
TV <-10
N<-50#_arr<-c(10,50,100)
p_number<-60#P<-c(20,40,60,80,100)

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

    
    
    data_train$event[censored_before] <-"1"
    data_train$event <-as.numeric(data_train$event)
    col_names <-list(1:ncol(data_test))
    col_names <-paste0("","A",col_names[[1]],"")
    colnames(data_test) <-col_names
    colnames(data_test)[length(colnames(data_test))-1] <- "time"
    colnames(data_test)[length(colnames(data_test))] <- "event"
    data_test$event <-as.numeric(data_test$event)
    
    # Create the gg_survival object
    gg_dta <- gg_survival(interval="time",
                          censor="event",
                          data=data_train,
                          conf.int=0.95)
    
    #create a folder to contain the results
    
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
    
    
    #train set train (default nodesize is 15 for survival family - the paper used 3)
    rfsrc_train <- rfsrc(Surv(time,event) ~ .,
                         ntree = 500,
                         nodesize = 3,
                         data=data_train,
                         nsplit=10, 
                         na.action="na.impute",
                         tree.err=TRUE, 
                         importance=TRUE)
    print(rfsrc_train)
    # would it be more accurate to measure the prediction accuracy by running the model with entire train data set?  
    
    rfsrc_train_CV <- predict(rfsrc_train, 
                              newdata = data_train,
                              na.action = "na.impute",
                              importance = TRUE)
    
    
    #test set predictions  
    rfsrc_test <- predict(rfsrc_train, 
                          newdata = data_test,
                          na.action = "na.impute",
                          importance = TRUE)
    
    print(rfsrc_test)

    
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
    vimp<-gg_vimp(nvar = 40, rfsrc_train)
    vimp<-gsub("A", "", vimp$vars)
    vimp_count<-length(which(as.numeric(vimp[1:TV])<=10))
    precision_var<-length(which(as.numeric(vimp[1:TV])<=10))/TV
    
    plot(gg_vimp(nvar = 40, rfsrc_train))+
      theme(legend.position=c(p=0.8,0.2))+
      labs(fill="VIMP > 0")
    
    # minimal depth - nodesize different, threshold different
    
    varsel_train <- var.select(rfsrc_train)
    gg_md <- gg_minimal_depth(varsel_train)
    
   plot(gg_md)
    #test set predictions
    
    
    #Prediction Accuracy 
    #find cut-off time(median survival time)
    t<- median(data_train$time)
    m <- which.min(abs(rfsrc_train$time.interest - t))
    t<- rfsrc_train$time.interest[m]  
    
    t2<- median(data_test$time)
    m2 <- which.min(abs(rfsrc_test$time.interest - t2))
    t2<- rfsrc_test$time.interest[m2]  
    #
    # Get the ground truth as a vector of who did survive beyond t
    truth <- data_train$time[-censored_before] > t
    truth2 <- data_test$time > t2
    #
    # Get the prediction of who rf thinks will survive > t
    pred_train <- rfsrc_train$survival.oob[,m] >0.5 #rfsrc_train$survival[,m] > 0.5
    pred_train <-pred_train[-censored_before]
    pred_test <- rfsrc_test$survival[,m] >0.5 #rfsrc_train$survival.oob[,m] >0.5
    
    # Confusion matrix
    cm_train <- matrix(c(length(which(truth=="FALSE" & pred_train=="FALSE")),length(which(truth=="FALSE" & pred_train=="TRUE")),
                         length(which(truth=="TRUE" & pred_train=="FALSE")),length(which(truth=="TRUE" & pred_train=="TRUE"))),
                       ncol=2,byrow=TRUE)
    colnames(cm_train) <- c("FALSE","TRUE")
    rownames(cm_train) <- c("FALSE","TRUE")
    cm_train <- as.table(cm_train)
    
    cm_test <- matrix(c(length(which(truth2=="FALSE" & pred_test=="FALSE")),length(which(truth2=="FALSE" & pred_test=="TRUE")),
                        length(which(truth2=="TRUE" & pred_test=="FALSE")),length(which(truth2=="TRUE" & pred_test=="TRUE"))),
                      ncol=2,byrow=TRUE)
    colnames(cm_test) <- c("FALSE","TRUE")
    rownames(cm_test) <- c("FALSE","TRUE")
    cm_test <- as.table(cm_test)
    
    # Accuracy  
    accuracy_train <- (cm_train[1,1] + cm_train[2,2])/sum(cm_train)
    accuracy_test <-(cm_test[1,1] + cm_test[2,2])/sum(cm_test)
    #collect data in an array(replace 'a'th number in the array after each loop)
    Err_train[a]<<- accuracy_train
    Err_test[a]<<- accuracy_test
    precision_var[a]<<-precision_var
    

  #reset the "a" value to keep it =<3
  #collect average accuary values of the model at different P values
  #create dataframes and combine with old data
 