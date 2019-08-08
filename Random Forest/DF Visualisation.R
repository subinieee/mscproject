library(readr)
library(openxlsx)
library(dplyr)
library(directlabels)


#load data
rsf_data<-read.xlsx("~/GitHub/mscproject/Data/RSF_4. Missing data.xlsx")
sp_data <-read.xlsx("~/GitHub/mscproject/Data/SP_4. Missing data.xlsx")
cox_data <-read.xlsx("~/GitHub/mscproject/Data/CPH_4. Missing data.xlsx")


#graphtype PA-CM(prediction accuracy-Confusion matrix), PA-CI(Concordance Index),FS(Feature Selection)

graphtype <-"sf"

#Reshaping data

if(graphtype=='PA-CM'){
  precision_var<-merge(rsf_data,sp_data, by=c(1:9),all = TRUE)
  precision_var<-merge(precision_var,cox_data, by=c(1:9),all = TRUE)
  colnames(precision_var)[11]<-"RSF"
  colnames(precision_var)[15]<-"BMR"
  colnames(precision_var)[19]<-"CPH"
  
  #restructure and aggregate data
  Vis_df <- melt(precision_var, id.vars = c('NA.'), measure.vars = c("RSF","BMR","CPH"))
  ggplot(Vis_df, aes(x = as.factor(NA.), y = value, fill= variable)) + 
    geom_boxplot()+
    geom_point(alpha=0.3,size = 2,position=position_dodge(0.75)) +
    geom_hline(yintercept=0.5, linetype="dashed", 
               color = "red", size=0.3)+
    scale_colour_manual(values=c('steel blue','orange','grey')) +
    scale_linetype_manual(values=c("solid","solid","dotted")) +
    scale_fill_manual(values=c('steel blue','orange','grey')) +
    labs(y = "C-Index", x = "True Features(%)")+
    scale_x_discrete(breaks=c(0,5,10,20,40))+
    #geom_smooth(method = "lm", alpha = .15, aes(fill = variable, linetype=variable),size = 0.9)+ 
    theme_bw()+
    theme(plot.title = element_text(size=14, face="bold.italic"),
          axis.title.x = element_text(size=13, face="bold"),
          axis.title.y = element_text(size=13, face="bold"),
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11),
          legend.position = "none")+
    facet_wrap(facets = 'variable')+
    theme(strip.text.x = element_text(size=11, face='bold'))+
    ylim(0,1)
  
  
  } else if(graphtype=='PA-CI'){
    precision_var<-merge(rsf_data,sp_data, by=c(1:9),all = TRUE)
    precision_var<-merge(precision_var,cox_data, by=c(1:9),all = TRUE)
    colnames(precision_var)[10]<-"RSF"
    colnames(precision_var)[14]<-"BMR"
    colnames(precision_var)[18]<-"CPH"

    #restructure and aggregate data
    Vis_df <- melt(precision_var, id.vars = c('NA.'), measure.vars = c("RSF","BMR","CPH"))
    ggplot(Vis_df, aes(x = as.factor(NA.), y = value, fill=variable)) + 
      geom_boxplot()+
      geom_jitter(alpha=0.3,size = 2,position=position_jitter(0)) +
      scale_colour_manual(values=c('steel blue','orange','black')) +
      scale_linetype_manual(values=c("solid","solid","dotted")) +
      scale_fill_manual(values=c('steel blue','orange','dark grey')) +
      geom_hline(yintercept=0.5, linetype="dashed", 
                       color = "red", size=0.3)+
      labs(y = "C-Index", x = "Missing Data(%)")+
      #scale_x_continuous(breaks=c(1,3,5,7,9))+
      geom_smooth(method = "lm", alpha = .15, aes(fill = variable, linetype=variable),size = 0.9)+ 
      theme_bw()+
      theme(plot.title = element_text(size=14, face="bold.italic"),
            axis.title.x = element_text(size=13,face="bold"),
            axis.title.y = element_text(size=13, face="bold"),
            axis.text.x = element_text(size=11,face="bold"),
            axis.text.y = element_text(size=11,face="bold"), 
            legend.position = "none",
            legend.text=element_text(size=11),
            legend.title = element_blank())+
      facet_wrap(facets = 'variable')+
      theme(strip.text.x = element_text(size=11, face='bold'))+
      ylim(0,1)
      } else{
      precision_var<-merge(rsf_data,sp_data, by=c(1:9),all = TRUE)
      precision_var<-merge(precision_var,cox_data, by=c(1:9),all = TRUE)
      colnames(precision_var)[12]<-"RSF"
      colnames(precision_var)[16]<-"BMR"
    
    #restructure and aggregate data
      Vis_df <- melt(precision_var, id.vars = c('NA.'), measure.vars = c("RSF",
                                                                            "BMR"))
      cbPalette <- c("#006633", "#E69F00", "black", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      #Vis_df$TV <-as.factor(Vis_df$TV)
      #best fitted line with 95% confidence interval in log scale.
      ggplot(Vis_df, aes(x = as.factor(NA.), y = value, fill=variable)) + 
        geom_boxplot()+
        geom_point(alpha=0.3,size = 2) +
        scale_colour_manual(values=c('steel blue','orange')) +
        scale_linetype_manual(values=c("solid","solid")) +
        scale_fill_manual(values=c('steel blue','orange')) +
        labs(y = "Precision", x = "Missing Data(%)")+
        #scale_x_continuous(breaks=c(1,3,5,7,9))+
        #geom_smooth(method = "lm", alpha = .15, aes(fill = variable, linetype=variable),size = 0.9) + 
        theme_bw()+
        theme(plot.title = element_text(size=14, face="bold.italic"),
              axis.title.x = element_text(size=13,face="bold"),
              axis.title.y = element_text(size=13, face="bold"),
              axis.text.x = element_text(size=11,face="bold"),
              axis.text.y = element_text(size=11,face="bold"),
              legend.position = "none",
              legend.text=element_text(size=11),
              legend.title = element_blank())+
        facet_wrap(facets = 'variable')+
        theme(strip.text.x = element_text(size=11, face='bold'))+
        ylim(0,1)
      }

#Save graph    
ggsave(paste0("4. FS(b).png"), plot=last_plot(),path="~/GitHub/mscproject/Data/")



