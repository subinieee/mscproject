

#2 way anova (when +2 groups with 2 independent variable)
precision_var<-merge(rsf_data,cox_data, by=c(1:9),all = TRUE)
colnames(precision_var)[10]<-"RSF"
colnames(precision_var)[14]<-"CPH"
Vis_df <- melt(precision_var, id.vars = c('TV','P'), measure.vars = c("RSF","CPH"))
res.aov2<-aov(value~variable,data=Vis_df)
summary(res.aov2)

precision_var<-merge(sp_data,cox_data, by=c(1:9),all = TRUE)
colnames(precision_var)[10]<-"SP"
colnames(precision_var)[14]<-"CPH"
Vis_df <- melt(precision_var, id.vars = c('TV','P'), measure.vars = c("SP","CPH"))
res.aov2<-aov(value~variable,data=Vis_df)
summary(res.aov2)