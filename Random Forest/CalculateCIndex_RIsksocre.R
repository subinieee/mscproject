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
  
  for (j in 2:length(Covariate)){
    # add more terms on
    s <- s + data[,Covariate[j]] * weights$Weight[j]
  }
  
  # subtract constant
  s <- s - weights$Weight[length(weights$Covariate)]
  
  return(s)
}

#Load data

riskSignatureDataframe <- read.table(file.choose(), sep = '*', col.names = c("Weight", "Covariate"), 
                                     stringsAsFactors = F, skip = 2, fill = T)

data <-read.table(file.choose(), sep = '', skip=3,
                  stringsAsFactors = F, fill = T, nrows=50)

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






calculateBenefitScore <- function(benefitSignatureDataframe, data){
  #param riskSignature The data frame of the risk signature
  #param data The data frame that contains the covariates 
  #return s The vector of risk scores, one for each row of data
  
  # Hint, load from a weights file like this:
  # riskSignatureDataframe <- read.table("risk_signature.dat",
  #                                  sep = '*', col.names = c("Weight", "Covariate"), 
  #                                  stringsAsFactors = F, skip = 5, fill = T)
  
  
  library(stringr)
  
  weights <- benefitSignatureDataframe
  weights$Weight <- str_remove(weights$Weight, "\\+ \\(")
  weights$Weight <- str_remove(weights$Weight, "\\- \\(")
  weights$Weight <- str_remove(weights$Weight, "\\(")
  weights$Weight <- str_remove(weights$Weight, "\\)")
  weights$Covariate <- str_remove_all(weights$Covariate, "\\(")
  weights$Covariate <- str_remove_all(weights$Covariate, "\\)")
  weights$Covariate <- str_remove_all(weights$Covariate, " ")
  weights$Covariate <- sub(":", ".", weights$Covariate)
  weights$Weight <- as.numeric(weights$Weight)
  
  weights <- weights[c(2,1)]
  
  # List of covariates used
  Covariate <- weights$Covariate
  
  # check presence of covariates in data
  for (j in 1:length(Covariate)){
    if(!(Covariate[j] %in% colnames(data)))
      stop(paste("ERROR: Covariate", Covariate[j], "missing from data."))
  }
  
  j=1
  # match col in data to jth name in Covariate vector
  # multiply that col by the jth weight
  s <- data[,Covariate[j]] * weights$Weight[j]
  
  for (j in 2:length(Covariate)){
    # add more terms on
    s <- s + data[,Covariate[j]] * weights$Weight[j]
  }

  return(s)
}

calculateTreatmentResponseScore <- function(benefitSignatureDataframe, data){
  #param riskSignature The data frame of the risk signature
  #param data The data frame that contains the covariates 
  #return s The vector of risk scores, one for each row of data
  
  # Hint, load from a weights file like this:
  # riskSignatureDataframe <- read.table("risk_signature.dat",
  #                                  sep = '*', col.names = c("Weight", "Covariate"), 
  #                                  stringsAsFactors = F, skip = 2, fill = T)
  
  
  library(stringr)
  
  weights <- benefitSignatureDataframe
  
  weights$Weight <- str_remove(weights$Weight, "S= \\(")
  weights$Weight <- str_remove(weights$Weight, "\\+ \\(")
  weights$Weight <- str_remove(weights$Weight, "\\- \\(")
  weights$Weight <- str_remove(weights$Weight, "\\(")
  weights$Weight <- str_remove(weights$Weight, "\\)")
  
  # Remove the multiplier that apppears in this type of file.
  weights$Covariate <- str_remove_all(weights$Covariate, "\\(.*\\)x") 
  
  weights$Covariate <- str_remove_all(weights$Covariate, "\\(")
  weights$Covariate <- str_remove_all(weights$Covariate, "\\)")
  weights$Covariate <- str_remove_all(weights$Covariate, " ")
  weights$Covariate <- sub(":", ".", weights$Covariate)
  weights$Weight <- as.numeric(weights$Weight)
  
  weights <- weights[c(2,1)]
  
  # List of covariates used
  Covariate <- weights$Covariate
  
  # check presence of covariates in data
  for (j in 1:length(Covariate)){
    if(!(Covariate[j] %in% colnames(data)))
      stop(paste("ERROR: Covariate", Covariate[j], "missing from data."))
  }
  
  j=1
  # match col in data to jth name in Covariate vector
  # multiply that col by the jth weight
  s <- data[,Covariate[j]] * weights$Weight[j]
  
  for (j in 2:length(Covariate)){
    # add more terms on
    s <- s + data[,Covariate[j]] * weights$Weight[j]
  }
  
  return(s)
}
