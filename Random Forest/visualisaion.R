

#linear plot
data <- df1

data2 <- melt(data, id.vars = c('p_n'), measure.vars = c('test','train','Sensitivity','Specificity'))
ggplot(data2, aes(x = p_n, y = value, color = variable) ) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .15, aes(fill = variable))+ theme_bw()+
  facet_wrap(~variable)