library(readr)
library(openxlsx)
N_arr<-c(10,50,100)
P_arr<-c(20,40,60,80,100) 
TV<-10
nrtestdata<-2
nrdata<-1


excelpath<-"~/GitHub/mscproject/Data/EOT/SP_results_N10.xlsx"
wb<-createWorkbook()
addWorksheet(wb, "P_N")
addWorksheet(wb, "TV_P")
saveWorkbook(wb,excelpath, overwrite = FALSE)

for (j in c(1:3)){
N<-N_arr[j]
for (i in c(1:5)){P<-P_arr[i]
datasrc_train<-paste0("~/GitHub/mscproject/Data/EOT/N",N,"_P20-100_TV10(",nrdata,")/sd",P,"/SETCV_L2/confusion_table.txt")
datasrc_test<-paste0("~/GitHub/mscproject/Data/EOT/N",N,"_P20-100_TV10(",nrdata,")/Test(",nrtestdata,")/sd",P,"_test/confusion_table_test.txt")
act_CoV<-read.csv(paste0("~/GitHub/mscproject/Data/EOT/N",N,"_P20-100_TV10(",nrdata,")/sd",P,"/SETCV_L2/active_covariates.txt"),sep = "",header = FALSE)
act_CoV<-paste(act_CoV[(nrow(act_CoV)-(TV-1)),(2:(TV+1))])
CoV_count<-length(which(as.numeric(act_CoV[1:TV])<=10))
precision_<-length(which(as.numeric(act_CoV[1:TV])<=10))/TV


conf_train <-read.table(text = gsub("/", "\t", read_lines(datasrc_train)), sep=(""), header = FALSE, nrows = 2)
conf_test <-read.table(text = gsub("/", "\t", read_lines(datasrc_test)), sep=(""), header = FALSE, nrows = 2)



a<-conf_train$V7[1]+conf_train$V11[2]
b<-conf_train$V8[1]+conf_train$V12[2]
accuracy_train <-a/b

c<-conf_test$V7[1]+conf_test$V11[2]
d<-conf_test$V8[1]+conf_test$V12[2]
accuracy_test <-c/d

Err_train<- accuracy_train
Err_test<- accuracy_test 
precision_var<- precision_
#create dataframes and combine with old data
df1<<-data.frame(dataset=paste0("P",P,"_N",N,"_TV10(",nrdata,")"), testdata=nrtestdata,p_n=P/N, test=Err_test, train=Err_train, precision_var=precision_var, stringsAsFactors=FALSE)
df1<<-rbind(read.xlsx(excelpath,sheet="P_N"), df1)
df2<<-data.frame(dataset=paste0("P",P,"_N",N,"_TV10(",nrdata,")"), testdata=nrtestdata,TV_P=TV/P, test=Err_test, train=Err_train,precision_var=precision_var, stringsAsFactors=FALSE)
df2<<-rbind(read.xlsx(excelpath,sheet = "TV_P"), df2)
#push combined data to a new excel file. 
wb<-createWorkbook()
addWorksheet(wb, "P_N")
writeData(wb,sheet="P_N",df1)
addWorksheet(wb, "TV_P")
writeData(wb,sheet="TV_P",df2)
saveWorkbook(wb,excelpath, overwrite = TRUE)

}



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
ggsave(paste0("SP predacc_p_n.png"), plot=last_plot(),path="~/GitHub/mscproject/Data/")

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
ggsave(paste0("SP predacc_TV_P.png"), plot=last_plot(),path="~/GitHub/mscproject/Data/")

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
ggsave(paste0("SP select_p_n.png"), plot=last_plot(),path="~/GitHub/mscproject/Data/")

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
ggsave(paste0("SP select_TV_P.png"), plot=last_plot(),path="~/GitHub/mscproject/Data/")

