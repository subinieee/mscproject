library(readr)
library(openxlsx)
P<-c(120,140) 
for (i in c(1:2)){
sourcedata<-paste0("/Users/subinieee/Documents/GitHub/mscproject/Data/N200_P120-200_TV18_R1/sd",P[i],"/SETCV_L2/confusion_table.txt")
testt <-read.csv(sourcedata, sep=(" "), header = FALSE)
a<-testt$V11[1]
b<-testt$V15[2]
dfe <-data.frame(trial=paste0("=",a), test=paste0("=",b), stringsAsFactors=FALSE)
dfe<-rbind(read.xlsx("/Users/subinieee/Desktop/ggRFSRC plot/test.xlsx"), dfe)

write.xlsx(dfe,"/Users/subinieee/Desktop/ggRFSRC plot/test.xlsx")
}

