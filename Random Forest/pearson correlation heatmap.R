# Get lower triangle of the correlation matrix
get_lower_tri<-function(df){
  df[upper.tri(df)] <- NA
  return(df)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(df){
  df[lower.tri(df)]<- NA
  return(df)
}
upper_tri <- get_upper_tri(df)
upper_tri
# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
