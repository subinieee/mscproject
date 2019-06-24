library(readr)
library(openxlsx)
N_arr<-c(10,50,100)
P<-c(20,40,60,80,100) 
TV<-10
Err_train<-c(1:5)
Err_test<-c(1:5)
Sensitivity_train <-c(1:5)
Specificity_train <-c(1:5)
excelpath<-"~/GitHub/mscproject/Data/SP_results.xlsx"
wb<-createWorkbook()
addWorksheet(wb, "P_N")
addWorksheet(wb, "TV_P")
addWorksheet(wb, "TV_N")
saveWorkbook(wb,excelpath, overwrite = TRUE)

for (j in c(1:3)){
N<-N_arr[j]
for (i in c(1:5)){
datasrc_train<-paste0("~/GitHub/mscproject/Data/train data/confusion table_train/confusion_table_n",N,"_p",P[i],"_tv",TV,".txt")
datasrc_test<-paste0("~/GitHub/mscproject/Data/test data/confusion table_test/confusion_table_n",N,"_p",P[i],"_tv",TV,"_test.txt")

act_CoV<-read.csv(paste0("~/GitHub/mscproject/Data/N",N,"_P20-100_TV",TV,"/sd",P[i],"/SETCV_L2/active_covariates.txt"),sep = "",header = FALSE)
act_CoV<-paste(act_CoV[(nrow(act_CoV)-(TV-1)),(2:(TV+1))])
CoV_count<-length(which(as.numeric(act_CoV[1:TV])<=10))
Sensitivity<-length(which(as.numeric(act_CoV[1:TV])<=10))/TV
Specificity<-(P[i]-TV-length(which(as.numeric(act_CoV[1:TV])>10)))/(P[i]-TV)


conf_train <-read.table(text = gsub("/", "\t", read_lines(datasrc_train)), sep=(""), header = FALSE, nrows = 2)
conf_test <-read.table(text = gsub("/", "\t", read_lines(datasrc_test)), sep=(""), header = FALSE, nrows = 2)



a<-conf_train$V7[1]+conf_train$V11[2]
b<-conf_train$V8[1]+conf_train$V12[2]
accuracy_train <-a/b

c<-conf_test$V7[1]+conf_test$V11[2]
d<-conf_test$V8[1]+conf_test$V12[2]
accuracy_test <-c/d

Err_train[i]<- accuracy_train
Err_test[i]<- accuracy_test 
Sensitivity_train[i]<- Sensitivity
Specificity_train[i]<- Specificity
}

#create dataframes and combine with old data

df1 <-data.frame(p_n=log10(P/N), test=Err_test, train=Err_train, sensitivity=Sensitivity_train, specificity=Specificity_train, stringsAsFactors=FALSE)
df1<-rbind(read.xlsx(excelpath,sheet="P_N"), df1)
df2 <-data.frame(TV_P=log10(TV/P), test=Err_test, train=Err_train, sensitivity=Sensitivity_train, specificity=Specificity_train, stringsAsFactors=FALSE)
df2<-rbind(read.xlsx(excelpath,sheet = "TV_P"), df2)
df3 <-data.frame(TV_N=log10(TV/N), test=Err_test, train=Err_train, sensitivity=Sensitivity_train, specificity=Specificity_train, stringsAsFactors=FALSE)
df3<-rbind(read.xlsx(excelpath, sheet="TV_N"), df3)

#push combined data to a new excel file. 
write.xlsx(df1,excelpath,sheetName="P_N", append=FALSE)
write.xlsx(df2,excelpath,sheetName="TV_P",append=TRUE)
write.xlsx(df3,excelpath,sheetName="TV_N", append=TRUE)
wb<-createWorkbook()
addWorksheet(wb, "P_N")
writeData(wb,sheet="P_N",df1)
addWorksheet(wb, "TV_P")
writeData(wb,sheet="TV_P",df2)
addWorksheet(wb, "TV_N")
writeData(wb,sheet="TV_N",df3)
saveWorkbook(wb,excelpath, overwrite = TRUE)
}

#linear plot


Vis_df1 <- melt(df1, id.vars = c('p_n'), measure.vars = c('test','train','sensitivity','specificity'))
ggplot(Vis_df1, aes(x = p_n, y = value, color = variable) ) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .15, aes(fill = variable))+ theme_bw()+
  facet_wrap(~variable)
ggsave(paste0("SP_p_n.png"), plot=last_plot(),path="~/GitHub/mscproject/Data/")

Vis_df2 <- melt(df2, id.vars = c('TV_P'), measure.vars = c('test','train','sensitivity','specificity'))
ggplot(Vis_df2, aes(x = TV_P, y = value, color = variable) ) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .15, aes(fill = variable))+ theme_bw()+
  facet_wrap(~variable)
ggsave(paste0("SP_TV_P.png"), plot=last_plot(),path="~/GitHub/mscproject/Data/")

Vis_df3 <- melt(df3, id.vars = c('TV_N'), measure.vars = c('test','train','sensitivity','specificity'))
ggplot(Vis_df3, aes(x = TV_N, y = value, color = variable) ) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .15, aes(fill = variable))+ theme_bw()+
  facet_wrap(~variable)
ggsave(paste0("SP_TV_N.png"), plot=last_plot(),path="~/GitHub/mscproject/Data/")

