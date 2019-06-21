library(randomForestSRC)
library(dplyr)
library(ggplot2)
library(ggRandomForests)
library(parallel)
library(caret)
library(readr)
library(openxlsx)

Err_train <-c(1:3)
Err_Test <-c(1:3)
Avg_Err_train<-c(1:5)
Avg_Err_Test<-c(1:5)
a<-1
b<-1
TV <-10
N <-50
P<-c(10,20,30,40,50)
datasrc <-"~/GitHub/mscproject/Data/EOTy_N100_P10-50_TV10_R1"

#Function for repeating RF with different datasets on a loop.
rfsrc_in_loop <- function(sampledata){
  savepath<<-paste0("",datasrc,"/sd",sampledata,"")
  for (j in c(1:3)){
# Data cleaning (read .dat file and remove index column)
  data1 <-read.delim(paste0(savepath,"/SD",sampledata,".dat"), 
                     skip=3, 
                     sep=(" "),
                     nrows=N, 
                     header = FALSE, 
                     stringsAsFactors = FALSE)
  set.seed(j)
  data2 <- data1[,-1]

#column names
  #colnames(data2) <-st.cols
  col_names <-list(1:ncol(data2))
  col_names <-paste0("","A",col_names[[1]],"")
  colnames(data2)[length(colnames(data2))-1] <- "time"
  colnames(data2)[length(colnames(data2))] <- "event"
  data2$event <-as.numeric(data2$event)

# Create the gg_survival object
  gg_dta <- gg_survival(interval="time",
                      censor="event",
                      data=data2,
                      conf.int=0.95)

#create a folder to contain the results
  dir.create(path=paste0("",datasrc,sampledata,"/RSF",sampledata,""))

# Survival probability vs observation time
  plot(gg_dta) +
    labs(y="Survival Probability", 
       x="Observation Time(years)",
       color="Treatment", 
       fill="treatment") +
    theme(legend.position=c(0.2,0.2)) +
    coord_cartesian(y=c(0,1.01))

  ggsave("surv_obstime.png", plot=last_plot(),path=savepath)

#cumulative hazard vs obs time
  plot(gg_dta, type = "cum_haz") +
    labs(y = "Cumulative Hazard",
         x = "Observation Time (years)",
         color = "Treatment", fill = "Treatment") +
    theme(legend.position = c(0.2, 0.8)) + 
    coord_cartesian(ylim = c(-0.02, 1.22))

  ggsave("cum_hazard.png", plot=last_plot(),path=savepath)
  
#train set train (default nodesize is 15 for survival family - the paper used 3)
  rfsrc_train <- rfsrc(Surv(time,event) ~ .,
                       ntree = 1000,
                       nodesize = 3,
                       data=data2,
                       nsplit=10, 
                       na.action="na.impute",
                       sampsize=25,
                       tree.err=TRUE, 
                       importance=TRUE)

  
  capture.output(summary(rfsrc_train),file=paste0("",savepath,"/summary_train.txt",""))
  print(rfsrc_train)
  capture.output(print(rfsrc_train),file=paste0("",savepath,"/print_train.txt",""))
 
#Generalisation error
  gg_error1<-gg_error(rfsrc_train)
  plot(na.omit(gg_error1))  

  ggsave("OOB_err.png", plot=last_plot(),path=savepath)
       
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

    ggsave("rfsrc_train.png", plot=last_plot(),path=savepath)

# variable importance(lbls=st.labs needs to be defined)

  plot(gg_vimp(nvar = P[i], rfsrc_train))+
    theme(legend.position=c(p=0.8,0.2))+
   labs(fill="VIMP > 0")

  ggsave(paste0("vimp(",j,").png"), plot=last_plot(),path=savepath)

# minimal depth - nodesize different, threshold different

  varsel_train <- var.select(rfsrc_train)
  gg_md <- gg_minimal_depth(varsel_train)

  capture.output(print(gg_md), file=paste0(savepath,"/top_variables(",j,").txt",""))

  plot(gg_md)
  ggsave(paste0("top_variables(",j,").png"), plot=last_plot(),path=savepath)
#test set predictions

  
#Prediction Accuracy 
  #find cut-off time(median survival time)
  t<- median(data2$time)
  m <- which.min(abs(rfsrc_train$time.interest - t))
  t<- rfsrc_train$time.interest[m]  
  #
  # Get the ground truth as a vector of who did survive beyond t
  truth <- data2$time > t
   #
  # Get the prediction of who rf thinks will survive > t
  pred_train <- rfsrc_train$survival[,m] >0.5 #rfsrc_train$survival[,m] > 0.5
  pred_test <- rfsrc_train$survival.oob[,m] >0.5 #rfsrc_train$survival.oob[,m] >0.5
  # Confusion matrix
  cm_train <- matrix(c(length(which(truth=="FALSE" & pred_train=="FALSE")),length(which(truth=="FALSE" & pred_train=="TRUE")),
                 length(which(truth=="TRUE" & pred_train=="FALSE")),length(which(truth=="TRUE" & pred_train=="TRUE"))),
               ncol=2,byrow=TRUE)
  colnames(cm_train) <- c("FALSE","TRUE")
  rownames(cm_train) <- c("FALSE","TRUE")
  cm_train <- as.table(cm_train)
  
  cm_test <- matrix(c(length(which(truth=="FALSE" & pred_test=="FALSE")),length(which(truth=="FALSE" & pred_test=="TRUE")),
                  length(which(truth=="TRUE" & pred_test=="FALSE")),length(which(truth=="TRUE" & pred_test=="TRUE"))),
                ncol=2,byrow=TRUE)
  colnames(cm_test) <- c("FALSE","TRUE")
  rownames(cm_test) <- c("FALSE","TRUE")
  cm_test <- as.table(cm_test)
  
  # Accuracy  
  accuracy_train <- (cm_train[1,1] + cm_train[2,2])/sum(cm_train)
  accuracy_test <-(cm_test[1,1] + cm_test[2,2])/sum(cm_test)
  #collect data in an array(replace 'a'th number in the array after each loop)
  Err_train[a]<<- accuracy_train
  Err_Test[a]<<- accuracy_test
  a <<- a+1
  sampledata<<-sampledata
  }
  #reset the "a" value to keep it =<3
  a<<-1
  #collect average accuary values of the model at different P values
  Avg_Err_train[b]<<- mean(x = Err_train)
  Avg_Err_Test[b]<<- mean(x=Err_Test)
  b<<-b+1
}

# Run rfsrc for different datasets on the loop 
for (i in c(1:5)){rfsrc_in_loop(P[i])}

#create dataframes and combine with old data
excelpath<-paste0("",datasrc,"/RSF_N",N,"_results.xlsx","")
df1 <-data.frame(p_n=P/N, test=Avg_Err_Test, train=Avg_Err_train, stringsAsFactors=FALSE)
df1<-rbind(read.xlsx(excelpath,sheet="P_N"), df1)
df2 <-data.frame(TV_P=TV/P, test=Avg_Err_Test, train=Avg_Err_train, stringsAsFactors=FALSE)
df2<-rbind(read.xlsx(excelpath,sheet = "TV_P"), df2)
df3 <-data.frame(TV_N=TV/N, test=Avg_Err_Test, train=Avg_Err_train, stringsAsFactors=FALSE)
df3<-rbind(read.xlsx(excelpath, sheet="TV_N"), df3)

#push combined data to a new excel file. 
write.xlsx(df1,excelpath,sheetName="P_N", append=FALSE)
write.xlsx(df2,excelpath,sheetName="TV_P",append=TRUE)
write.xlsx(df2,excelpath,sheetName="TV_N", append=TRUE)
wb<-createWorkbook()
addWorksheet(wb, "P_N")
writeData(wb,sheet="P_N",df1)
addWorksheet(wb, "TV_P")
writeData(wb,sheet="TV_P",df2)
addWorksheet(wb, "TV_N")
writeData(wb,sheet="TV_N",df3)
saveWorkbook(wb,excelpath, overwrite = TRUE)

