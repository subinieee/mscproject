library(randomForestSRC)
library(dplyr)
library(ggplot2)
library(ggRandomForests)
library(parallel)
library(caret)
library(readr)
library(openxlsx)

#Function for repeating RF with different datasets on a loop.
#make sure the filepath and nrow for data1 are adjusted according to the datafile
N <-53

st.cols<- read.delim("C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Daiichi_All_PFS.names", sep=(" "), header = FALSE, stringsAsFactors = FALSE)[,2]

# Data cleaning (read .dat file and remove index column)
  data1 <-read.delim("C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Daiichi_All_PFS.dat", 
                     skip=3, 
                     sep=(" "),
                     nrows=N, 
                     header = FALSE, 
                     stringsAsFactors = FALSE)
  set.seed(100)
  data2 <- data1[,-1]

#column names
  colnames(data2) <-st.cols
  colnames(data2)[length(colnames(data2))-1] <- "time"
  colnames(data2)[length(colnames(data2))] <- "event"
  data2$event <-as.numeric(data2$event)

# Create the gg_survival object
  gg_dta <- gg_survival(interval="time",
                      censor="event",
                      data=data2,
                      conf.int=0.95)

#creat a folder to contain the results
  dir.create(path="C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF")

# Survival probability vs observation time
  plot(gg_dta) +
    labs(y="Survival Probability", 
       x="Observation Time(years)",
       color="Treatment", 
       fill="treatment") +
    theme(legend.position=c(0.2,0.2)) +
    coord_cartesian(y=c(0,1.01))

  ggsave("surv_obstime.png", plot=last_plot(),path="C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF")

#cumulative hazard vs obs time
  plot(gg_dta, type = "cum_haz") +
    labs(y = "Cumulative Hazard",
         x = "Observation Time (years)",
         color = "Treatment", fill = "Treatment") +
    theme(legend.position = c(0.2, 0.8)) + 
    coord_cartesian(ylim = c(-0.02, 1.22))

  ggsave("cum_hazard.png", plot=last_plot(),path="C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF")
  
#train set train (default nodesize is 15 for survival family - the paper used 3)
  rfsrc_train <- rfsrc(Surv(time,event) ~ .,
                       ntree = 1000,
                       nodesize = 3,
                       data=data2,
                       nsplit=10, 
                       na.action="na.impute", 
                       tree.err=TRUE, 
                       importance=TRUE)

  
  capture.output(summary(rfsrc_train),file=paste0("C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF/summary_train.txt"))
  print(rfsrc_train)
  capture.output(print(rfsrc_train),file=paste0("C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF/print_train.txt"))
 
#Generalisation error
  gg_error1<-gg_error(rfsrc_train)
  plot(na.omit(gg_error1))  

  ggsave("OOB_err.png", plot=last_plot(),path="C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF")
       
#strCols should be defined "TRUE" for death(event), "FALSE" for censored
#strCol <- c("TRUE" = "red",
#            "FALSE"="blue")

  ggRFsrc <-plot(gg_rfsrc(rfsrc_train),alpha=0.2)+
    theme(legend.position="none")+
  #scale_color_manual(values = strCol)+
    labs(y="Survival Probability",
         x="Time(years)")+
    coord_cartesian(ylim=c(-0.01,1.01))
  show(ggRFsrc)

    ggsave("rfsrc_train.png", plot=last_plot(),path="C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF")

#test set predictions

  rfsrc_test <- predict(rfsrc_train, 
                            newdata = test,
                            na.action = "na.impute",
                            importance = TRUE)
  
  summary_test <- summary(rfsrc_test)
  capture.output(summary(rfsrc_test),file=paste0("C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF/summary_test.txt"))

  print_test<-print(rfsrc_test)
  capture.output(print(rfsrc_test),file=paste0("C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF/print_test.txt"))

  plot(gg_rfsrc(rfsrc_test),alpha=0.2)+
#  scale_color_manual(values = strCol)+
    theme(legend.position="none")+
    labs(y="Survival Probability",
         x="Time(years)")+
    coord_cartesian(ylim=c(-0.01,1.01))

  ggsave("rfsrc_test.png", plot=last_plot(),path="C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF/")
  
  # Choose a cut-off time
  #
  # Look at the vector of times which rfsrc produced predictions for
  # Maybe choose the middle time
  # m <- round(length(rfsrc_test$time.interest)/2)
  # Or closest to some chosen time
  #t <- 3.827397
  #m <- which.min(abs(rfsrc_pbc_test$time.interest - t))
  
  t<- median(train$time)
  
  m <- which.min(abs(rfsrc_train$time.interest - t))
  
  t<- rfsrc_train$time.interest[m]  

# Get the ground truth as a vector of who did survive beyond t
  truth <- train$time > t
  

# Get the prediction of who rf thinks will survive > t
  pred_test <- rfsrc_train$survival[,m] > 0.5
  pred_train <- rfsrc_train$survival.oob[,m] >0.5
# Confusion matrix
  cm_train <- matrix(c(length(which(truth2=="FALSE" & pred2=="FALSE")),length(which(truth2=="FALSE" & pred2=="TRUE")),
                  length(which(truth2=="TRUE" & pred2=="FALSE")),length(which(truth2=="TRUE" & pred2=="TRUE"))),
                ncol=2,byrow=TRUE)
  colnames(cm_train) <- c("FALSE","TRUE")
  rownames(cm_train) <- c("FALSE","TRUE")
  cm_train <- as.table(cm_train)
  
  cm_test <- matrix(c(length(which(truth=="FALSE" & pred=="FALSE")),length(which(truth=="FALSE" & pred=="TRUE")),
                 length(which(truth=="TRUE" & pred=="FALSE")),length(which(truth=="TRUE" & pred=="TRUE"))),
               ncol=2,byrow=TRUE)
  colnames(cm_test) <- c("FALSE","TRUE")
  rownames(cm_test) <- c("FALSE","TRUE")
  cm_test <- as.table(cm_test)
  
# Accuracy
    accuracy_train <-(cm_train[1,1] + cm_train[2,2])/sum(cm_train)
  accuracy_test <- (cm_test[1,1] + cm_test[2,2])/sum(cm_test)
  
# variable importance(lbls=st.labs needs to be defined)

  plot(gg_vimp(rfsrc_train), lbls=st.cols)+
    theme(legend.position=c(p=0.8,0.2))+
   labs(fill="VIMP > 0")

  ggsave("vimp.png", plot=last_plot(),path="C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF/")

# minimal depth - nodesize different, threshold different

  varsel_train <- var.select(rfsrc_train)#refit=TRUE, method="vh")
  gg_md <- gg_minimal_depth(varsel_train)

  capture.output(print(gg_md), file="C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF/top_variables.txt")

  plot(gg_md)
  ggsave("top_variables.png", plot=last_plot(),path=path="C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF/")

#variable selection comparison

  plot(gg_minimal_vimp(gg_md),lbls=st.cols)+
    theme(legend.position = c(0.8,0.2))
  ggsave("var_comparison.png", plot=last_plot(),path="C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF/")

#variable dependence

  ggRFsrc + geom_vline(aes(xintercept=1), 
                       linetype="dashed")+
    coord_cartesian(xlim=c(0,5))

  ggsave("variable_dependence.png", plot=last_plot(),path="C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF/")
  Err_train[a]<- accuracy_train
  Err_Test[a]<- accuracy_test

#Create workbook
  
wb<-createWorkbook()
addWorksheet(wb, "Train")
writeData(wb,sheet="Train",cm)
addWorksheet(wb, "Test")
writeData(wb,sheet="Test",cm2)
saveWorkbook(wb, "C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF/confusion_matrix.xlsx", overwrite = TRUE)
saveWorkbook(wb, "C:/Users/Public/bin/SaddlePoint-Signature-v2.8.7/Test/Daiichi/RF/confusion_matrix.txt", overwrite = TRUE)

