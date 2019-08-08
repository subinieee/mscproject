library(randomForestSRC)
library(dplyr)
library(ggplot2)
library(ggRandomForests)
library(parallel)
library(caret)
library(readr)
library(openxlsx)
library(reshape2)
library(survival)
library(survminer)
 

a<-1 
studynr<-4
study<- c('1. Dimension','2. Noise','3. Censoring','4. Missing data','5. Negative Control')
EOT<-'no EOT'
nrdata_arr<-c(1,2,3)
nrtestdata<-4
nadata<-20
TV <-10
N_arr<-c(100)
P_arr<-c(100)

excelpath_rsf<-paste0("~/GitHub/mscproject/Data/RSF_",study[studynr],".xlsx")
excelpath_sp<-paste0("~/GitHub/mscproject/Data/SP_",study[studynr],".xlsx")
excelpath_cox<-paste0("~/GitHub/mscproject/Data/CPH_",study[studynr],".xlsx")

wb<-createWorkbook()
saveWorkbook(wb,excelpath_rsf, overwrite = FALSE)
saveWorkbook(wb,excelpath_sp, overwrite = FALSE)
saveWorkbook(wb,excelpath_cox, overwrite = FALSE)

#loop for different train data
for(l in c(1:3)){nrdata<-nrdata_arr[l]
#loop for different N value
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
  Err_test <<-c(1:3)
  Varsel_sensitivity <<-c(1:3)
  Varsel_specificity <<-c(1:3)
  cindex_rsf <<-c(1:3) 
  savepath<<-paste0("",datasrc,"/RSF",P)
  
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
  col_names <-paste0("","covariate",col_names[[1]],"")
  colnames(data_train) <-col_names
  colnames(data_train)[length(colnames(data_train))-1] <- "time"
  colnames(data_train)[length(colnames(data_train))] <- "event"
  data_train$event <-as.numeric(data_train$event)
  data_train<<-data_train
  
  #
  col_names <-list(1:ncol(data_test))
  col_names <-paste0("","covariate",col_names[[1]],"")
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

#test set predictions  
  rfsrc_test <<- predict(rfsrc_train, 
                        newdata = data_test,
                        na.action = "na.impute",
                        importance = TRUE)
  
  capture.output(summary(rfsrc_test),file=paste0("",savepath,"/summary_test.txt",""))
  print(rfsrc_test)
  capture.output(print(rfsrc_test),file=paste0("",savepath,"/print_test.txt",""))
 
# variable importance

  vimp<-gg_vimp(rfsrc_train)
  vimp<-gsub("covariate", "", vimp$vars)
  vimp_count<-length(which(as.numeric(vimp[1:TV])<=TV))
  sensitivity_rate<-length(which(as.numeric(vimp[1:TV])<=TV))/TV
  specificity_rate<-length(which(as.numeric(vimp[(TV+1):P])>TV))/(P-TV)
  

  plot(gg_vimp(rfsrc_train))+
    theme(legend.position=c(p=0.8,0.2))+
    labs(fill="VIMP > 0")
  capture.output(print(vimp), file=paste0(savepath,"/vimp(",j,").txt",""))
  ggsave(paste0("vimp(",j,").png"), plot=last_plot(),path=savepath)

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
  
  #SP riskscore calculation
  calculateRiskScore <- function(riskSignatureDataframe, data){
    #param riskSignature The data frame of the risk signature
    #param data The data frame that contains the covariates 
    #return s The vector of risk scores, one for each row of data

    library(stringr)
    
    weights <- riskSignatureDataframe
    weights$Weight <- str_remove(weights$Weight, "S=\\(")
    weights$Weight <- str_remove(weights$Weight, "\\+ \\(")
    weights$Weight <- str_remove(weights$Weight, "\\- \\(")
    weights$Weight <- str_remove(weights$Weight, "\\)")
    weights$Covariate <- str_remove_all(weights$Covariate, "\\(")
    weights$Covariate <- str_remove_all(weights$Covariate, "\\)")
    weights$Covariate <- str_remove_all(weights$Covariate, " ")
    weights$Covariate <- str_remove_all(weights$Covariate, "_")
    weights$Weight <- as.numeric(weights$Weight)
    
    weights <- weights[c(2,1)]
    
    # List of covariates used
    Covariate <- weights$Covariate[1:(length(weights$Covariate)-1)]  # last row is the const
    
    # check presence of covariates in data
    for (j in 1:length(Covariate)){
      if(!(Covariate[j] %in% colnames(data)))
        stop(paste("ERROR: Covariate", Covariate[j], "missing from data."))
    }
    
    j=1
    # match col in data to jth name in Covariate vector
    # multiply that col by the jth weight
    s <- data[,Covariate[j]] * weights$Weight[j]
    if (length(Covariate)>1){
      for (j in 2:length(Covariate)){
        # add more terms on
        s <- s + data[,Covariate[j]] * weights$Weight[j]
      }
    }
    
    # subtract constant
    s <- s - weights$Weight[length(weights$Covariate)]
    
    return(s)
  }
  
  
  #SP data
  cm_sp<-paste0(datasrc,"/test",nrtestdata-3,"/sd",P,"/confusion_table.txt")
  riskSignatureDataframe <- read.table(paste0(datasrc,"/sd",P,"/RiskScore_formula.txt"), sep = '*', col.names = c("Weight", "Covariate"), 
                                       stringsAsFactors = F, skip = 2, fill = T)
  act_CoV<-read.csv(paste0(datasrc,"/sd",P,"/SETCV_L2/active_covariates.txt"),sep = "",header = FALSE)

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
 
  #Cox as baseline
  library(e1071)
  
  data<-e1071::impute(data_test,what ="mean")
  data<-as.data.frame(data)
  data2<-e1071::impute(data_train,what="mean")
  data2<-as.data.frame(data2)
  fit<-coxph(Surv(time,event)~.,data2,method='breslow',x=TRUE)
  baseH1 <- predictCox(fit, newdata = data , times = t2)
  y_pred <- baseH1$survival>0.5

  cm_cox <- matrix(c(length(which(truth2=="FALSE" & y_pred=="FALSE")),length(which(truth2=="FALSE" & y_pred=="TRUE")),
                      length(which(truth2=="TRUE" & y_pred=="FALSE")),length(which(truth2=="TRUE" & y_pred=="TRUE"))),
                    ncol=2,byrow=TRUE)
  colnames(cm_cox) <- c("FALSE","TRUE")
  rownames(cm_cox) <- c("FALSE","TRUE")
  cm_cox <- as.table(cm_cox)
  
  # Accuracy  
  accuracy_train <<- (cm_train[1,1] + cm_train[2,2])/sum(cm_train)
  accuracy_test <<-(cm_test[1,1] + cm_test[2,2])/sum(cm_test)
  accuracy_cox <<-(cm_cox[1,1] + cm_cox[2,2])/sum(cm_cox)
  #collect data in an array(replace 'a'th number in the array after each loop)
  Err_test[a]<<- accuracy_test
  Varsel_sensitivity[a]<<- sensitivity_rate
  Varsel_specificity[a]<<-specificity_rate
  cindex_rsf[a] <<- 1-rfsrc_test$err.rate[1000]
  P<<-P
  a<<-a+1
  }
  #reset the "a" value to keep it =<3
  #collect average accuary values of the model at different P values
  #create dataframes and combine with old data
  
  #impute missing data
  library(e1071)
  data<-e1071::impute(data_test,what="mean")
  #calculate risk score
  riskscore <- calculateRiskScore(riskSignatureDataframe = riskSignatureDataframe, data = data)
  data<-as.data.frame(data)
  #C-index for SP
  surv<-Surv(data$time, data$event)
  riskscore <- -(riskscore)
  concordancetest<-concordance(surv~riskscore)
  concordance_sp<-concordancetest[["count"]][["concordant"]]+concordancetest[["count"]][["tied.xy"]]
  tied <-concordancetest[["count"]][["tied.x"]]+concordancetest[["count"]][["tied.y"]]
  cindex_sp <- (concordance_sp+tied)/sum(concordancetest$count)
  
  #variable selection
  act_CoV<-paste(act_CoV[(nrow(act_CoV)-(TV-1)),(2:(TV+1))])
  CoV_count<-length(which(as.numeric(act_CoV[1:TV])<=TV))
  sensitivity_sp<-length(which(as.numeric(act_CoV[1:TV])<=TV))/TV
  specificity_sp<-1-length(which(as.numeric(act_CoV)>TV))/(P-TV)
  
  conf_test <-read.table(text = gsub("/", "\t", read_lines(cm_sp)), sep=(""), header = FALSE, nrows = 2)
  c<-conf_test$V7[1]+conf_test$V11[2]
  d<-conf_test$V8[1]+conf_test$V12[2]
  accuracy_sp <-c/d
  
  
  #C-index for CPH
  cindex_cox<-concordance(-data_test$time ~ baseH1$cumhazard,na.action=na.exclude)$concordance
  
  
  
  df1<<-data.frame("N"=N, "P"=P,"TV"=TV, 'traindata'=nrdata, 'testdata'=nrtestdata, 'NA'=nadata,'EOT'=EOT, 'p_n'=P_arr[i]/N, 'TV_P'=TV/P_arr[i], 'RSFCIndex'=mean(cindex_rsf),'prediction accuracy'=mean(Err_test), 'Sensitivity'=mean(Varsel_sensitivity), 'Specificity'=mean(Varsel_specificity), stringsAsFactors=FALSE)
  df1<<-rbind(read.xlsx(excelpath_rsf), df1)
  df2<<-data.frame("N"=N, "P"=P,"TV"=TV, 'traindata'=nrdata, 'testdata'=nrtestdata, 'NA'=nadata,'EOT'=EOT, 'p_n'=P_arr[i]/N,'TV_P'=TV/P_arr[i],'SPCIndex'=cindex_sp, 'prediction accuracy'=accuracy_sp, 'Sensitivity'=sensitivity_sp, 'Specificity'=specificity_sp, stringsAsFactors=FALSE)
  df2<<-rbind(read.xlsx(excelpath_sp), df2)
  df3<<-data.frame("N"=N, "P"=P,"TV"=TV, 'traindata'=nrdata, 'testdata'=nrtestdata, 'NA'=nadata,'EOT'=EOT, 'p_n'=P_arr[i]/N,'TV_P'=TV/P_arr[i],'CoxCIndex'=cindex_cox, 'prediction accuracy'=accuracy_cox, stringsAsFactors=FALSE)
  df3<<-rbind(read.xlsx(excelpath_cox), df3)  
  
  #push combined data to a new excel file. 

  write.xlsx(df1,excelpath_rsf, overwrite = TRUE)
  write.xlsx(df2,excelpath_sp, overwrite = TRUE)
  write.xlsx(df3,excelpath_cox, overwrite = TRUE)
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

