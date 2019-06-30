library(randomForestSRC)
library(dplyr)
library(ggplot2)
library(ggRandomForests)
library(parallel)
library(caret)
library(readr)
library(openxlsx)


a<-1
excelpath<-"~/GitHub/mscproject/Data/RSF_results.xlsx"
wb<-createWorkbook()
addWorksheet(wb, "P_N")
addWorksheet(wb, "TV_P")
addWorksheet(wb, "TV_N")
saveWorkbook(wb,excelpath, overwrite = FALSE)

nrdata<-1
nrtestdata<-1
TV <-10
N_arr<-c(10,50,100)
P<-c(20,40,60,80,100)

for (k in c(1:3)){N<<-N_arr[k]
datasrc <-paste0("~/GitHub/mscproject/Data/N",N,"_P20-100_TV10(",nrdata,")")
#Function for repeating RF with different datasets on a loop.
rfsrc_in_loop <- function(p_number){
  Err_train <<-c(1:3)
  Err_test <<-c(1:3)
  precision_var <<-c(1:3)

  savepath<<-paste0("",datasrc,"/RSF",p_number,"")
  for (j in c(1:3)){
# Data cleaning (read .dat file and remove index column)
  set.seed(j)
  data_train <-read.delim(paste0("",datasrc,"/sd",p_number,"/SD",p_number,".dat"), 
                     skip=3, 
                     sep=(" "),
                     nrows=N, 
                     header = FALSE, 
                     stringsAsFactors = FALSE)
  data_train <- data_train[,-1]
  data_test <-read.delim(paste0("~/GitHub/mscproject/Data/test data(",nrtestdata,")/sd",p_number,"_test/SD",p_number,".dat"),
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
  #
  censored_before<-which(data_train$time[which(data_train$event==0)]<median(data_train$time))
  #
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
  dir.create(path=paste0("",datasrc,"/RSF",p_number,""))

# survival probability vs observation time
  plot(gg_dta) +
    labs(y="survival Probability", 
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
  rfsrc_train <<- rfsrc(Surv(time,event) ~ .,
                       ntree = 1000,
                       nodesize = 3,
                       data=data_train,
                       nsplit=10, 
                       na.action="na.impute",
                       tree.err=TRUE, 
                       importance=TRUE)
 capture.output(summary(rfsrc_train),file=paste0("",savepath,"/summary_train.txt",""))
  print(rfsrc_train)
  capture.output(print(rfsrc_train),file=paste0("",savepath,"/print_train.txt",""))  
# would it be more accurate to measure the prediction accuracy by running the model with entire train data set?  
  
  rfsrc_train_CV <<- predict(rfsrc_train, 
                        newdata = data_train,
                        na.action = "na.impute",
                        importance = TRUE)
  
 
#test set predictions  
  rfsrc_test <<- predict(rfsrc_train, 
                        newdata = data_test,
                        na.action = "na.impute",
                        importance = TRUE)
  
  capture.output(summary(rfsrc_test),file=paste0("",savepath,"/summary_test.txt",""))
  print(rfsrc_test)
  capture.output(print(rfsrc_test),file=paste0("",savepath,"/print_test.txt",""))
  
#save survival prediction data
if(j==1){
  wb2<-createWorkbook()
  addWorksheet(wb2, "Train_Survival")
  addWorksheet(wb2, "Train_Survival.oob")
  addWorksheet(wb2, "Train_CV_Survival")
  addWorksheet(wb2, "Test_Survival")
  saveWorkbook(wb2,paste0("",savepath,"/RSF_resultsDB.xlsx",""), overwrite = TRUE)}
  
  survivalDB<<-cbind(dataset=paste0("P",P[i],"_N",N_arr[k],"_TV10(",nrdata,")"),seed=j,rfsrc_train$survival)
  df4<<-rbind(read.xlsx(paste0("",savepath,"/RSF_resultsDB.xlsx",""),sheet="Train_Survival"), survivalDB)
  survivaloobDB<<-cbind(dataset=paste0("P",P[i],"_N",N_arr[k],"_TV10(",nrdata,")",seed=j),rfsrc_train$survival.oob)
  df5<<-rbind(read.xlsx(paste0("",savepath,"/RSF_resultsDB.xlsx",""),sheet = "Train_Survival.oob"), survivaloobDB)
  CVsurvivalDB<<-cbind(dataset=paste0("P",P[i],"_N",N_arr[k],"_TV10(",nrdata,")",seed=j),rfsrc_train_CV$survival)
  df6<<-rbind(read.xlsx(paste0("",savepath,"/RSF_resultsDB.xlsx",""), sheet="Train_CV_Survival"), CVsurvivalDB)
  testsurvivalDB<<-cbind(dataset=paste0("P",P[i],"_N",N_arr[k],"_TV10(",nrdata,")",seed=j), rfsrc_test$survival)
  df7<<-rbind(read.xlsx(paste0("",savepath,"/RSF_resultsDB.xlsx",""), sheet="Test_Survival"), testsurvivalDB)
  
  writeData(wb2,sheet="Train_Survival",df4)
  writeData(wb2,sheet="Train_Survival.oob",df5)
  writeData(wb2,sheet="Train_CV_Survival",df6)
  writeData(wb2,sheet="Test_Survival",df7)
  saveWorkbook(wb2,paste0("",savepath,"/RSF_resultsDB.xlsx",""), overwrite = TRUE)
  
#Generalisation error
  gg_error1<-gg_error(rfsrc_train)
  plot(na.omit(gg_error1))  

  ggsave("OOB_err.png", plot=last_plot(),path=savepath)
       
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

   ggsave("rfsrc_train.png", plot=last_plot(),path=savepath)

# variable importance(lbls=st.labs needs to be defined)
  vimp<-gg_vimp(nvar = P[i], rfsrc_train)
  vimp<-gsub("A", "", vimp$vars)
  vimp_count<-length(which(as.numeric(vimp[1:TV])<=10))
  precision_var<-length(which(as.numeric(vimp[1:TV])<=10))/TV

  plot(gg_vimp(nvar = P[i], rfsrc_train))+
    theme(legend.position=c(p=0.8,0.2))+
   labs(fill="VIMP > 0")
  capture.output(print(vimp), file=paste0(savepath,"/vimp(",j,").txt",""))
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
  t<- median(data_train$time)
  m <- which.min(abs(rfsrc_train$time.interest - t))
  t<- rfsrc_train$time.interest[m]  
  
  t2<- median(data_test$time)
  m2 <- which.min(abs(rfsrc_test$time.interest - t2))
  t2<- rfsrc_test$time.interest[m2]  
  #
  # Get the ground truth as a vector of who did survive beyond t
  truth <- data_train$time >t2
  if(length(censored_before)!=0){
    truth <- data_train$time[-censored_before] > t
    }
  truth2 <- data_test$time > t2
   #
  # Get the prediction of who rf thinks will survive > t
  pred_train <- rfsrc_train$survival.oob[,m] >0.5 #rfsrc_train$survival[,m] > 0.5
  if(length(censored_before)!=0){
    pred_train <-pred_train[-censored_before]
  }
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

  a <<- a+1
  p_number<<-p_number
  }
  #reset the "a" value to keep it =<3
  #collect average accuary values of the model at different P values
  #create dataframes and combine with old data
  
  df1<<-data.frame(dataset=paste0("P",P[i],"_N",N_arr[k],"_TV10(",nrdata,")"), testdata=nrtestdata, seed=c(1:3),p_n=P[i]/N_arr[k], test=Err_test, train=Err_train, precision_var=precision_var, stringsAsFactors=FALSE)
  df1<<-rbind(read.xlsx(excelpath,sheet="P_N"), df1)
  df2<<-data.frame(dataset=paste0("P",P[i],"_N",N_arr[k],"_TV10(",nrdata,")"), testdata=nrtestdata, seed=c(1:3),TV_P=TV/P[i], test=Err_test, train=Err_train,precision_var=precision_var, stringsAsFactors=FALSE)
  df2<<-rbind(read.xlsx(excelpath,sheet = "TV_P"), df2)
  df3<<-data.frame(dataset=paste0("P",P[i],"_N",N_arr[k],"_TV10(",nrdata,")"), testdata=nrtestdata, seed=c(1:3),TV_N=TV/N_arr[k], test=Err_test, train=Err_train, precision_var=precision_var, stringsAsFactors=FALSE)
  df3<<-rbind(read.xlsx(excelpath, sheet="TV_N"), df3)
  
  #push combined data to a new excel file. 
  wb<-createWorkbook()
  addWorksheet(wb, "P_N")
  writeData(wb,sheet="P_N",df1)
  addWorksheet(wb, "TV_P")
  writeData(wb,sheet="TV_P",df2)
  addWorksheet(wb, "TV_N")
  writeData(wb,sheet="TV_N",df3)
  saveWorkbook(wb,excelpath, overwrite = TRUE)
  a<<-1
}
# Run rfsrc for different datasets on the loop 
for (i in c(1:5)){rfsrc_in_loop(P[i])}
}


#linear plot


cbPalette <- c("#006633", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Vis_df1 <- melt(df1, id.vars = c('p_n'), measure.vars = c('test','train'))
ggplot(Vis_df1, aes(x = p_n, y = value, color = variable) ) +
  geom_point(aes(shape=variable),size = 3) +
  labs(y = "Prediction Accuracy", x = "Features/Data")+
  geom_smooth(method = "lm", alpha = .15, aes(fill = variable),size = 0.8)+ theme_bw()+
  theme(plot.title = element_text(size=14, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"))+
  scale_x_continuous(trans = 'log2') +
  ylim(NA,1)
ggsave(paste0("RSF predacc_p_n.png"), plot=last_plot(),path="~/GitHub/mscproject/Data/")

Vis_df2 <- melt(df2, id.vars = c('TV_P'), measure.vars = c('test','train'))
ggplot(Vis_df2, aes(x = TV_P, y = value, color = variable) ) +
  geom_point(aes(shape=variable),size = 3) +
  labs(y = "Prediction Accuracy", x = "True Covariates/Features")+
  geom_smooth(method = "lm", alpha = .15, aes(fill = variable),size = 0.8)+ theme_bw()+
  theme(plot.title = element_text(size=14, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"))+
  scale_x_continuous(trans = 'log2') +
  ylim(NA,1)
ggsave(paste0("RSF predacc_TV_P.png"), plot=last_plot(),path="~/GitHub/mscproject/Data/")

Vis_df1 <- melt(df1, id.vars = c('p_n'), measure.vars = c('precision_var'))
ggplot(Vis_df1, aes(x = p_n, y = value, color = variable) ) + 
  geom_point(aes(shape=variable),size = 3) +
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  labs(y = "Precision", x = "Features/Data")+
  geom_smooth(method = "lm", alpha = .15, aes(fill = variable),size = 0.8)+ theme_bw()+
  theme(plot.title = element_text(size=14, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"))+
  scale_x_continuous(trans = 'log2') +
  ylim(NA,1)
  ggsave(paste0("RSF select_p_n.png"), plot=last_plot(),path="~/GitHub/mscproject/Data/")

Vis_df2 <- melt(df2, id.vars = c('TV_P'), measure.vars = c('precision_var'))
ggplot(Vis_df2, aes(x = TV_P, y = value, color = variable) ) +
  geom_point(aes(shape=variable),size = 3) +
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  labs(y = "Precision", x = "True Covariates/Features")+
  geom_smooth(method = "lm", alpha = .15, aes(fill = variable),size = 0.8)+ theme_bw()+
  theme(plot.title = element_text(size=14, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"))+
  scale_x_continuous(trans = 'log2') +
  ylim(NA,1)
ggsave(paste0("RSF select_TV_P.png"), plot=last_plot(),path="~/GitHub/mscproject/Data/")

