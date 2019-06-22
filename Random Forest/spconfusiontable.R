library(readr)
library(openxlsx)
N<-100
P<-c(20,40,60,80,100) 
TV<-10

Err_train<-c(1:5)
Err_test<-c(1:5)

for (i in c(1:5)){
datasrc_train<-paste0("~/GitHub/mscproject/Data/Train data/confusion table_train/confusion_table_n",N,"_p",P[i],"_tv",TV,".txt")
datasrc_test<-paste0("~/GitHub/mscproject/Data/Test data/confusion table_test/confusion_table_n",N,"_p",P[i],"_tv",TV,"_test.txt")

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
}

#create dataframes and combine with old data
excelpath<-"~/GitHub/mscproject/Data/SP_results.xlsx"
df1 <-data.frame(p_n=P/N, test=Err_test, train=Err_train, stringsAsFactors=FALSE)
df1<-rbind(read.xlsx(excelpath,sheet="P_N"), df1)
df2 <-data.frame(TV_P=TV/P, test=Err_test, train=Err_train, stringsAsFactors=FALSE)
df2<-rbind(read.xlsx(excelpath,sheet = "TV_P"), df2)
df3 <-data.frame(TV_N=TV/N, test=Err_test, train=Err_train, stringsAsFactors=FALSE)
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
