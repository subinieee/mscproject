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
Err_Trial <-c(1:3)
Err_Test <-c(1:3)
a<-1
TV <-18
N <-200
P<-c(120,140,160) 
datasrc <-"/Users/subinieee/Desktop/ggRFSRC plot/N200_P120-200_TV18_R1/Data/SD"
rfsrc_in_loop <- function(sampledata){
# Data cleaning (read .dat file and remove index column)
  data1 <-read.delim(paste0(datasrc,sampledata,".dat"), 
                     skip=3, 
                     sep=(" "),
                     nrows=N, 
                     header = FALSE, 
                     stringsAsFactors = FALSE)
  set.seed(100)
  data2 <- data1[,-1]

#column names
  col_names <-list(1:ncol(data2))
  col_names <-paste0("","A",col_names[[1]],"")
  colnames(data2) <-col_names
  colnames(data2)[colnames(data2)==colnames(data2[ncol(data2)-1])] <- "time"
  colnames(data2)[colnames(data2)==colnames(data2[ncol(data2)])] <- "event"
  data2$event[data2$event=="2"] <-"0"
  data2$event <-as.numeric(data2$event)

# Create the gg_survival object
  gg_dta <- gg_survival(interval="time",
                      censor="event",
                      data=data2,
                      conf.int=0.95)

#split data in 50:50 (trial and test)
  seq_len(nrow(data2))
  smp_size <-floor(0.5*nrow(data2))
  train_ind<-sample(seq_len(nrow(data2)), size=smp_size)
  trial <- data2[train_ind,]
  test <- data2[-train_ind,]

#creat a folder to contain the results
  dir.create(path=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2))

# Survival probability vs observation time
  plot(gg_dta) +
    labs(y="Survival Probability", 
       x="Observation Time(years)",
       color="Treatment", 
       fill="treatment") +
    theme(legend.position=c(0.2,0.2)) +
    coord_cartesian(y=c(0,1.01))

  ggsave("surv_obstime.png", plot=last_plot(),path=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2))

#cumulative hazard vs obs time
  plot(gg_dta, type = "cum_haz") +
    labs(y = "Cumulative Hazard",
         x = "Observation Time (years)",
         color = "Treatment", fill = "Treatment") +
    theme(legend.position = c(0.2, 0.8)) + 
    coord_cartesian(ylim = c(-0.02, 1.22))

  ggsave("cum_hazard.png", plot=last_plot(),path=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2,"/"))
  
#Trial set train (default nodesize is 15 for survival family - the paper used 3)
  rfsrc_trial <- rfsrc(Surv(time,event) ~ .,
                       ntree = 1000,
                       nodesize = 3,
                       data=trial,
                       nsplit=10, 
                       na.action="na.impute", 
                       tree.err=TRUE, 
                       importance=TRUE)

  
  capture.output(summary(rfsrc_trial),file=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2,"/summary_trial.txt"))
  print(rfsrc_trial)
  capture.output(print(rfsrc_trial),file=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2,"/print_trial.txt"))
 
#Generalisation error
  gg_error1<-gg_error(rfsrc_trial)
  plot(na.omit(gg_error1))  

  ggsave("OOB_err.png", plot=last_plot(),path=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2))
       
#strCols should be defined "TRUE" for death(event), "FALSE" for censored
#strCol <- c("TRUE" = "red",
#            "FALSE"="blue")

  ggRFsrc <-plot(gg_rfsrc(rfsrc_trial),alpha=0.2)+
    theme(legend.position="none")+
  #scale_color_manual(values = strCol)+
    labs(y="Survival Probability",
         x="Time(years)")+
    coord_cartesian(ylim=c(-0.01,1.01))
  show(ggRFsrc)

    ggsave("rfsrc_trial.png", plot=last_plot(),path=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2))

#test set predictions

  rfsrc_test <- predict(rfsrc_trial, 
                            newdata = test,
                            na.action = "na.impute",
                            importance = TRUE)
  
  summary_test <- summary(rfsrc_test)
  capture.output(summary(rfsrc_test),file=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2,"/summary_test.txt"))

  print_test<-print(rfsrc_test)
  capture.output(print(rfsrc_test),file=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2,"/print_test.txt"))

  plot(gg_rfsrc(rfsrc_test),alpha=0.2)+
#  scale_color_manual(values = strCol)+
    theme(legend.position="none")+
    labs(y="Survival Probability",
         x="Time(years)")+
    coord_cartesian(ylim=c(-0.01,1.01))

  ggsave("rfsrc_test.png", plot=last_plot(),path=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2))
  
  # Choose a cut-off time
  #
  # Look at the vector of times which rfsrc produced predictions for
  # Maybe choose the middle time
  # m <- round(length(rfsrc_test$time.interest)/2)
  # Or closest to some chosen time
  #t <- 3.827397
  #m <- which.min(abs(rfsrc_pbc_test$time.interest - t))
  
  t <- median(test$time)
  t2<- median(trial$time)
  
  m <- which.min(abs(rfsrc_test$time.interest - t))
  m2 <- which.min(abs(rfsrc_trial$time.interest - t2))
  
  t <- rfsrc_test$time.interest[m]
  t2<- rfsrc_trial$time.interest[m2]  
  #
  # Get the ground truth as a vector of who did survive beyond t
  truth <- test$time > t
  truth2 <-trial$time >t2
  
  #
  # Get the prediction of who rf thinks will survive > t
  pred <- rfsrc_test$survival[,m] > 0.5
  pred2 <- rfsrc_trial$survival.oob[,m2] >0.5
  # Confusion matrix
  cm <- table(truth, pred)
  cm2 <-table(truth2, pred2)  
  # Accuracy
  cm <- matrix(c(length(which(truth=="FALSE" & pred=="FALSE")),length(which(truth=="FALSE" & pred=="TRUE")),
                 length(which(truth=="TRUE" & pred=="FALSE")),length(which(truth=="TRUE" & pred=="TRUE"))),
               ncol=2,byrow=TRUE)
  colnames(cm) <- c("FALSE","TRUE")
  rownames(cm) <- c("FALSE","TRUE")
  cm <- as.table(cm)
  
  cm2 <- matrix(c(length(which(truth2=="FALSE" & pred2=="FALSE")),length(which(truth2=="FALSE" & pred2=="TRUE")),
                  length(which(truth2=="TRUE" & pred2=="FALSE")),length(which(truth2=="TRUE" & pred2=="TRUE"))),
                ncol=2,byrow=TRUE)
  colnames(cm2) <- c("FALSE","TRUE")
  rownames(cm2) <- c("FALSE","TRUE")
  cm2 <- as.table(cm2)
  
  
  accuracy <- (cm[1,1] + cm[2,2])/sum(cm)
  accuracy2 <-(cm2[1,1] + cm2[2,2])/sum(cm2)
# variable importance(lbls=st.labs needs to be defined)

  plot(gg_vimp(rfsrc_trial))+
    theme(legend.position=c(p=0.8,0.2))+
    labs(fill="VIMP > 0")

  ggsave("vimp.png", plot=last_plot(),path=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2))

# minimal depth - nodesize different, threshold different

  varsel_trial <- var.select(rfsrc_trial)#refit=TRUE, method="vh")
  gg_md <- gg_minimal_depth(varsel_trial)

  capture.output(print(gg_md), file=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2,"/top_variables.txt"))

  plot(gg_md)
  ggsave("top_variables.png", plot=last_plot(),path=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2))

#variable selection comparison

  plot(gg_minimal_vimp(gg_md))+
    theme(legend.position = c(0.8,0.2))
  ggsave("var_comparison.png", plot=last_plot(),path=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2))

#variable dependence

  ggRFsrc + geom_vline(aes(xintercept=1), 
                       linetype="dashed")+
    coord_cartesian(xlim=c(0,5))

  ggsave("variable_dependence.png", plot=last_plot(),path=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2))
  Err_Trial[a]<<- accuracy2
  Err_Test[a]<<- accuracy
  a <<- a+1
  }
# Run rfsrc for different datasets on the loop 
for (i in c(1:3)){rfsrc_in_loop(P[i])}

df1 <-data.frame(p_n=P/N, test=Err_Test, trial=Err_Trial, stringsAsFactors=FALSE)
df1<-rbind(read.xlsx("/Users/subinieee/Desktop/ggRFSRC plot/test1.xlsx",sheet="P_N"), df1)
df2 <-data.frame(TV_P=TV/P, test=Err_Test, trial=Err_Trial, stringsAsFactors=FALSE)
df2<-rbind(read.xlsx("/Users/subinieee/Desktop/ggRFSRC plot/test1.xlsx",sheet = "TV_P"), df2)
df3 <-data.frame(TV_N=TV/N, test=Err_Test, trial=Err_Trial, stringsAsFactors=FALSE)
df3<-rbind(read.xlsx("/Users/subinieee/Desktop/ggRFSRC plot/test1.xlsx", sheet="TV_N"), df3)

write.xlsx(df1,"/Users/subinieee/Desktop/ggRFSRC plot/test1.xlsx",sheetName="P_N", append=FALSE)
write.xlsx(df2,"/Users/subinieee/Desktop/ggRFSRC plot/test1.xlsx",sheetName="TV_P",append=TRUE)
write.xlsx(df2,"/Users/subinieee/Desktop/ggRFSRC plot/test1.xlsx",sheetName="TV_N", append=TRUE)
wb<-createWorkbook()
addWorksheet(wb, "P_N")
writeData(wb,sheet="P_N",df1)
addWorksheet(wb, "TV_P")
writeData(wb,sheet="TV_P",df2)
addWorksheet(wb, "TV_N")
writeData(wb,sheet="TV_N",df3)
saveWorkbook(wb, "/Users/subinieee/Desktop/ggRFSRC plot/test1.xlsx", overwrite = TRUE)

