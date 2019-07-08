library(readr)
library(openxlsx)
library(dplyr)
library(directlabels)

#load data
rsf_data<-read.xlsx("~/GitHub/mscproject/Data/RSF_results.xlsx",sheet="TV_P")
sp_data <-read.xlsx("~/GitHub/mscproject/Data/SP_results.xlsx",sheet="TV_P")

#retain only unique coloumns
precision_var1<-distinct(as_tibble(cbind(rsf_data$TV_P,rsf_data$train, rsf_data$test)), .keep_all = TRUE)
precision_var2<-distinct(as_tibble(cbind(sp_data$TV_P,sp_data$train, sp_data$test)), .keep_all = TRUE)
precision_var<-merge(precision_var1,precision_var2, by="V1",all = TRUE)
colnames(precision_var)<-c("TV_P","RFSRC","SP")

#restructure and aggregate data
Vis_df <- melt(precision_var, id.vars = c('TV_P'), measure.vars = c("RFSRC","SP"))

#best fitted line with 95% confidence interval in log scale.
ggplot(Vis_df, aes(x = TV_P, y = value, color = variable)) +
  geom_point(aes(shape=variable),size = 2, alpha=0.3) +
  scale_colour_manual(values=c("steel blue","orange"))+
  scale_fill_manual(values=c("steel blue","orange"))+
 labs(y = "Precision", x = "TV/p")+
  geom_smooth(method = "lm", alpha = .15, aes(fill = variable,linetype= variable),size = 0.8)+
  scale_linetype_manual(values=c("solid","solid"))+
  theme_bw()+
  theme(plot.title = element_text(size=14, face="bold.italic"),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=11,face = "bold"),
        axis.text.y = element_text(size=11,face="bold"),
        legend.text=element_text(size=11),
        legend.title = element_blank())+
  scale_x_continuous(trans = 'log2') +
  ylim(0,1)
 


