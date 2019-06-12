library(randomForestSRC)
library(dplyr)
library(ggplot2)
library(ggRandomForests)
library(parallel)
library(caret)

#Function for repeating RF with different datasets on a loop.
#make sure the filepath and nrow for data1 are adjusted according to the datafile

rfsrc_in_loop <- function(sampledata){

#Data cleaning (read .dat file and remove index column)
  data1 <-read.delim(paste0("/Users/subinieee/Desktop/ggRFSRC plot/N100_P20-100_TV18_R1/Data/SD",sampledata,".dat"), skip=3, sep=(" "),nrows=100, header = FALSE, stringsAsFactors = FALSE)
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

# variable importance(lbls=st.labs needs to be defined)

  plot(gg_vimp(rfsrc_trial))+
    theme(legend.position=c(p=0.8,0.2))+
    labs(fill="VIMP > 0")

  ggsave("vimp.png", plot=last_plot(),path=paste0("/Users/subinieee/Desktop/ggRFSRC plot/SD",ncol(data2)-2))

# minimal depth - nodesize different, threshold different

  varsel_trial <- var.select(rfsrc_trial)
  varsel_trial
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
  }

# Run rfsrc for different datasets on the loop 
nr_p<-c(20,30,40,50,100) 
for (i in c(1:5)){rfsrc_in_loop(nr_p[i])}

