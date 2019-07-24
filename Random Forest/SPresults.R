library(readr)
library(openxlsx)
N_arr<-c(50,100)
P_arr<-c(20,40,60,80,100) 
TV<-10
nrtestdata<-2
nrdata<-1


excelpath<-"~/GitHub/mscproject/Data/no EOT/SP_results_N10.xlsx"
wb<-createWorkbook()
addWorksheet(wb, "P_N")
addWorksheet(wb, "TV_P")
saveWorkbook(wb,excelpath, overwrite = FALSE)
calculateRiskScore <- function(riskSignatureDataframe, data){
  #param riskSignature The data frame of the risk signature
  #param data The data frame that contains the covariates 
  #return s The vector of risk scores, one for each row of data
  
  # Hint, load from a weights file like this:
  # riskSignatureDataframe <- read.table("risk_signature.dat",
  #                                  sep = '*', col.names = c("Weight", "Covariate"), 
  #                                  stringsAsFactors = F, skip = 2, fill = T)
  
  
  library(stringr)
  
  weights <- riskSignatureDataframe
  weights$Weight <- str_remove(weights$Weight, "S=\\(")
  weights$Weight <- str_remove(weights$Weight, "\\+ \\(")
  weights$Weight <- str_remove(weights$Weight, "\\- \\(")
  weights$Weight <- str_remove(weights$Weight, "\\)")
  weights$Covariate <- str_remove_all(weights$Covariate, "\\(")
  weights$Covariate <- str_remove_all(weights$Covariate, "\\)")
  weights$Covariate <- str_remove_all(weights$Covariate, " ")
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


for (k in c(1:2)){
N<-N_arr[k]
for (i in c(1:5)){P<-P_arr[i]
#Load data
learning_validation<-read.delim(paste0("~/GitHub/mscproject/Data/no EOT/N",N,"_P20-100_TV10(",nrdata,")/sd",P,"/SETCV_L2/OverfittingCurves_risk1_reg2_norm1_data2.txt"),header=FALSE,sep="")
riskSignatureDataframe <- read.table(paste0("~/GitHub/mscproject/Data/no EOT/N",N,"_P20-100_TV10(",nrdata,")/sd",P,"/RiskScore_formula.txt"), sep = '*', col.names = c("Weight", "Covariate"), 
                                     stringsAsFactors = F, skip = 2, fill = T)

data <-read.table(paste0("~/GitHub/mscproject/Data/no EOT/N",N,"_P20-100_TV10(",nrdata,")/sd",P,"/SD",P,".dat"), sep = '', skip=3,
                  stringsAsFactors = F, fill = T, nrows=N)
act_CoV<-read.csv(paste0("~/GitHub/mscproject/Data/no EOT/N",N,"_P20-100_TV10(",nrdata,")/sd",P,"/SETCV_L2/active_covariates.txt"),sep = "",header = FALSE)

#data formatting
data<-data[,-1]
col_names <-list(1:ncol(data))
col_names <-paste0("","covariate",col_names[[1]],"")
colnames(data) <-col_names
colnames(data)[length(colnames(data))-1] <- "time"
colnames(data)[length(colnames(data))] <- "event"

#impute missing data
library(e1071)
data<-e1071::impute(data,what="mean")

#calculate risk score
riskscore <- calculateRiskScore(riskSignatureDataframe = riskSignatureDataframe, data = data)
data<-as.data.frame(data)

# Change Risk Score into C-Index
library(survival)
surv<-Surv(data$time, data$event)
riskscore <- -(riskscore)
concordancetest<-concordance(surv~riskscore)
concordance<-concordancetest[["count"]][["concordant"]]+concordancetest[["count"]][["tied.xy"]]
tied <-concordancetest[["count"]][["tied.x"]]+concordancetest[["count"]][["tied.y"]]


cindex <- (concordance+tied)/sum(concordancetest$count)
paste("C-Index score:",cindex)


#variable selection
act_CoV<-paste(act_CoV[(nrow(act_CoV)-(TV-1)),(2:(TV+1))])
CoV_count<-length(which(as.numeric(act_CoV[1:TV])<=10))
precision_var<-length(which(as.numeric(act_CoV[1:TV])<=10))/TV
prediction_accuracy <-learning_validation[length(riskSignatureDataframe$Covariate)-1,2]


#create dataframes and combine with old data
df1<<-data.frame(dataset=paste0("P",P,"_N",N,"_TV10(",nrdata,")"),'p_n'=P/N, 'prediction accuracy'=prediction_accuracy, 'var_sensitivity'=precision_var, 'C-Index'=cindex, stringsAsFactors=FALSE)
df1<<-rbind(read.xlsx(excelpath,sheet="P_N"), df1)
df2<<-data.frame(dataset=paste0("P",P,"_N",N,"_TV10(",nrdata,")"),'TV_P'=TV/P, 'prediction accuracy'=prediction_accuracy, 'var_sensitivity'=precision_var, 'C-Index'=cindex, stringsAsFactors=FALSE)
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

Vis_df1 <- melt(df1, id.vars = c('p_n'), measure.vars = c('prediction.accuracy','C.Index'))
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

Vis_df2 <- melt(df2, id.vars = c('TV_P'), measure.vars = c('prediction accuracy','C-index'))
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

Vis_df1 <- melt(df1, id.vars = c('p_n'), measure.vars = c('var_sensitivity'))
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

Vis_df2 <- melt(df2, id.vars = c('TV_P'), measure.vars = c('var_sensitivity'))
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

