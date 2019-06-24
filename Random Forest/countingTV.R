
library(readr)
library(openxlsx)
library(utils)
TV<-10
VIMPT<-read.table("/Users/subinieee/GitHub/mscproject/Data/train data/N10_P20-100_TV10/RSF20/vimp(2).txt")
VIMPT<-gsub("A", "", VIMPT$vars)
VIMPT<-read.table(text = gsub("A", "\t", read_lines(VIMPT)), sep="", nrows =TV)
   vimpt<-as.numeric(VIMPT[1:TV])
true_count <-length(which(VIMPT<=10))/TV
