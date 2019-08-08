library(randomForestSRC)
library(dplyr)
library(ggplot2)
library(ggRandomForests)
library(parallel)
library(caret)
library(readr)
library(openxlsx)
library(reshape2)
 


a<-1 
studynr<-1
study<- c('1. Dimension','2. Noise','3. Censoring','4. Missing data','5. Negative Control')
EOT<-'EOT'
nrdata_arr<-c(1,2,3)
nrtestdata<-6
nadata<-0
TV <-10
N_arr<-c(100)
P_arr<-c(100)
P<-100
excelpath<-paste0("~/GitHub/mscproject/Data/RSF_",study[studynr],".xlsx")
wb<-createWorkbook()
addWorksheet(wb, "P_N")
addWorksheet(wb, "TV_P")
saveWorkbook(wb,excelpath, overwrite = FALSE)
for(l in c(1:3)){nrdata<-nrdata_arr[l]
for (k in c(1:length(N_arr))){N<-N_arr[k]
if(studynr==1 | studynr== 5){
  datasrc <-paste0("~/GitHub/mscproject/Data/",study[studynr],"/N",N,"_P20-100_TV",TV,"(",nrdata,")")
  datasrc2 <-paste0("~/GitHub/mscproject/Data/",study[studynr],"/N",N,"_P20-100_TV",TV,"(",nrtestdata,")")
  } else if(studynr==2){
  datasrc <-paste0("~/GitHub/mscproject/Data/",study[studynr],"/N",N,"_P100_TV",TV,"(",nrdata,")")
  datasrc2 <-paste0("~/GitHub/mscproject/Data/",study[studynr],"/N",N,"_P100_TV",TV,"(",nrtestdata,")")
  } else if(studynr==3){
  datasrc <-paste0("~/GitHub/mscproject/Data/",study[studynr],"/",EOT,"/N",N,"_P100_TV",TV,"(",nrdata,")")
  datasrc2 <-paste0("~/GitHub/mscproject/Data/",study[studynr],"/",EOT,"/N",N,"_P100_TV",TV,"(",nrtestdata,")")
  } else{
  datasrc <-paste0("~/GitHub/mscproject/Data/",study[studynr],"/NA",nadata,"/N",N,"_P100_TV",TV,"(",nrdata,")")
  datasrc2 <-paste0("~/GitHub/mscproject/Data/",study[studynr],"/NA",nadata,"/N",N,"_P100_TV",TV,"(",nrtestdata,")")
  }


#Function for repeating RF with different datasets on a loop.
rfsrc_in_loop <- function(P){P<-P_arr[i]
  Err_train <<-c(1:3)
  Err_test <<-c(1:3)
  Varsel_sensitivity <<-c(1:3)
  Varsel_specificity <<-c(1:3)
  hunt_vimp<<-c(1:3)
  cindex <<-c(1:3) 

  savepath<<-paste0("",datasrc,"/RSF100")
  for (j in c(1:3)){
# Data cleaning (read .dat file and remove index column)
  set.seed(j)
  data_train <-read.delim(paste0("",datasrc,"/sd",P,"/SD",P,".dat"), 
                     skip=3, 
                     sep=(" "),
                     nrows=N, 
                     header = FALSE, 
                     stringsAsFactors = FALSE)
  data_train <- data_train[,-1]
  data_test <-read.delim(paste0("",datasrc2,"/sd",P,"/SD",P,".dat"),
                         skip=3, 
                         sep=(" "),
                         nrows=N, 
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
  data_train<<-data_train
  
  #
  col_names <-list(1:ncol(data_test))
  col_names <-paste0("","A",col_names[[1]],"")
  colnames(data_test) <-col_names
  colnames(data_test)[length(colnames(data_test))-1] <- "time"
  colnames(data_test)[length(colnames(data_test))] <- "event"
  data_test$event <-as.numeric(data_test$event)
  data_test<<-data_test

  #
  censored_before <<- which(data_train$event==0 & data_train$time<median(data_train$time))
  censored_before2 <<- which(data_test$event==0 & data_test$time<median(data_test$time))
  
  
  # Create the gg_survival object
  gg_dta <- gg_survival(interval="time",
                      censor="event",
                      data=data_train,
                      conf.int=0.95)

#create a folder to contain the results
  dir.create(path=paste0("",datasrc,"/RSF",P,""))

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
 
   # variable importance(lbls=st.labs needs to be defined)
  #vimp<-gg_vimp(rfsrc_train)
  #vimp_positive<-gsub("A", "", vimp$vars[which(vimp$vimp>0.002)])
  #vimp_negative<-gsub("A", "", vimp$vars[which(vimp$vimp<=0.002)])
  #sensitivity_rate<- length(which(as.numeric(vimp_positive)<=TV))/TV
  #specificity_rate<-length(which(as.numeric(vimp_positive)>TV))/P
  vimp<-gg_vimp(rfsrc_train)
  vimp<-gsub("A", "", vimp$vars)
  vimp_count<-length(which(as.numeric(vimp[1:TV])<=TV))
  sensitivity_rate<-length(which(as.numeric(vimp[1:TV])<=TV))/TV
  specificity_rate<-length(which(as.numeric(vimp[(TV+1):P])>TV))/(P-TV)
  

  
  
  plot(gg_vimp(rfsrc_train))+
    theme(legend.position=c(p=0.8,0.2))+
    labs(fill="VIMP > 0")
  capture.output(print(vimp), file=paste0(savepath,"/vimp(",j,").txt",""))
  ggsave(paste0("vimp(",j,").png"), plot=last_plot(),path=savepath)

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
   ggsave("rfsrc_train.png", plot=last_plot(),path=savepath)



# minimal depth - nodesize different, threshold different

  #varsel_train <- var.select(rfsrc_train, method = "md",refit=TRUE)
  
  #vh_vimp<-gsub("A", "", varsel_train$topvars)
  #vh_var<-length(which(as.numeric(vh_vimp[1:TV])<=TV))/TV
  
#test set predictions

  
#Prediction Accuracy 
#find cut-off time(median survival time)
  while(length(censored_before)!="0"){data_train<-data_train[-censored_before,]
  censored_before<<-which(data_train$event==0 & data_train$time<median(data_train$time))}
  t<- median(data_train$time)
  m <- which.min(abs(rfsrc_train$time.interest - t))
  t<- rfsrc_train$time.interest[m]  
  
  while(length(censored_before2)!="0"){data_test<-data_test[-censored_before2,]
  censored_before2<<-which(data_test$event==0 & data_test$time<median(data_test$time))}
  t2<- median(data_test$time)
  m2 <- which.min(abs(rfsrc_test$time.interest - t2))
  t2<- rfsrc_test$time.interest[m2]  
  #
  # Get the ground truth as a vector of who did survive beyond t
  truth <- data_train[which(data_train$event=="1"),]$time >t
  truth2 <- data_test[which(data_test$event=="1"),]$time >t2
   #
  # Get the prediction of who rf thinks will survive > t
  pred_train <- rfsrc_train$survival.oob[,m] >0.5 #rfsrc_train$survival[,m] > 0.5
  pred_train<-pred_train[which(data_train$event=="1")]
  pred_test <- rfsrc_test$survival[,m2] >0.5 #rfsrc_train$survival.oob[,m] >0.5
  pred_test<-pred_test[which(data_test$event=="1")]
  
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
  accuracy_train <<- (cm_train[1,1] + cm_train[2,2])/sum(cm_train)
  accuracy_test <<-(cm_test[1,1] + cm_test[2,2])/sum(cm_test)
  #collect data in an array(replace 'a'th number in the array after each loop)
  Err_train[a]<<- accuracy_train
  Err_test[a]<<- accuracy_test
  Varsel_sensitivity[a]<<- sensitivity_rate
  Varsel_specificity[a]<<-specificity_rate
  cindex[a] <<- 1-rfsrc_test$err.rate[1000]

  P<<-P
  a<<-a+1
  }
  #reset the "a" value to keep it =<3
  #collect average accuary values of the model at different P values
  #create dataframes and combine with old data
  
  df1<<-data.frame( "N"=N, "P"=P,"TV"=TV, 'traindata'=nrdata, 'testdata'=nrtestdata, 'NA'=nadata,'EOT'=EOT, 'p_n'=P_arr[i]/N, 'test'=mean(Err_test), 'train'=mean(Err_train), 'Sensitivity'=mean(Varsel_sensitivity), 'Specificity'=mean(Varsel_specificity), 'C-Index'=mean(cindex), stringsAsFactors=FALSE)
  df1<<-rbind(read.xlsx(excelpath,sheet="P_N"), df1)
  df2<<-data.frame("N"=N, "P"=P,"TV"=TV, 'traindata'=nrdata, 'testdata'=nrtestdata, 'NA'=nadata,'EOT'=EOT,'TV_P'=TV/P_arr[i], test=mean(Err_test), train=mean(Err_train),'Sensitivity'=mean(Varsel_sensitivity),'Specificity'=mean(Varsel_specificity), 'C-Index'=mean(cindex), stringsAsFactors=FALSE)
  df2<<-rbind(read.xlsx(excelpath,sheet = "TV_P"), df2)
  
  #push combined data to a new excel file. 
  wb<-createWorkbook()
  addWorksheet(wb, "P_N")
  writeData(wb,sheet="P_N",df1)
  addWorksheet(wb, "TV_P")
  writeData(wb,sheet="TV_P",df2)
  saveWorkbook(wb,excelpath, overwrite = TRUE)
  a<<-1
}
# Run rfsrc for different datasets on the loop 
for (i in c(1:length(P_arr))){rfsrc_in_loop(P[i])}

}
}


#linear plot


cbPalette <- c("#006633", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Vis_df1 <- melt(df1, id.vars = c('p_n'), measure.vars = c('test','train','C.Index'))
ggplot(Vis_df1, aes(x = p_n, y = value, color = variable) ) +
  geom_point(aes(shape=variable),size = 3) +
  labs(y = "Prediction Accuracy", x = "Features/Data")+
  geom_smooth(method = "lm", alpha = .15, aes(fill = variable),size = 0.8)+ theme_bw()+
  theme(plot.title = element_text(size=14, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"))+
  scale_x_continuous(trans = 'log2') +
  ylim(0,1)

ggsave(paste0("RSF predacc_p_n_N50.png"), plot=last_plot(),path="~/GitHub/mscproject/Data/1. Dimension/")

Vis_df2 <- melt(df2, id.vars = c('TV_P'), measure.vars = c('test','train','C.Index','variable_hunt'))
ggplot(Vis_df2, aes(x = TV_P, y = value, color = variable) ) +
  geom_point(aes(shape=variable),size = 3) +
  labs(y = "Prediction Accuracy", x = "True Covariates/Features")+
  geom_smooth(method = "lm", alpha = .15, aes(fill = variable),size = 0.8)+ theme_bw()+
  theme(plot.title = element_text(size=14, face="bold.italic"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"))+
  scale_x_continuous(trans = 'log2') +
  ylim(0,1)
ggsave(paste0("RSF predacc_TV_P.png"), plot=last_plot(),path="~/GitHub/mscproject/Data/EOT/")

Vis_df1 <- melt(df1, id.vars = c('p_n'), measure.vars = c('Sensitivity','Specificity'))
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
  ylim(0,1)
  ggsave(paste0("RSF select_p_n.png"), plot=last_plot(),path="~/GitHub/mscproject/Data/EOT/")

Vis_df2 <- melt(df2, id.vars = c('TV_P'), measure.vars = c('precision.var'))
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
  ylim(0,1)
ggsave(paste0("RSF select_TV_P.png"), plot=last_plot(),path="~/GitHub/mscproject/Data/EOT/")

