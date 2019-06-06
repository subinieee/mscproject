library(randomForestSRC)
library(dplyr)
library(ggplot2)
library(ggRandomForests)
library(parallel)
library(plot3D)
set.seed(100)

#Data cleaning (read .dat file and remove index column)
data1 <-read.delim(file.choose(), sep=(" "),header = FALSE)
data2 <- data1[,-1]

#column names
col_names <-list(1:52)
col_names <-paste0("","A",col_names[[1]],"")
colnames(data2) <-col_names
colnames(data2)[colnames(data2)=="A51"] <- "time"
colnames(data2)[colnames(data2)=="A52"] <- "event"

#split 50:50 (trial and test)
seq_len(nrow(data2))
smp_size <-floor(0.5*nrow(data2))
train_ind<-sample(seq_len(nrow(data2)), size=smp_size)
trial <- data2[train_ind,]
test <- data2[-train_ind,]

# Create the gg_survival object
gg_dta <- gg_survival(interval="time",
                      censor="event",
                      data=data2,
                      conf.int=0.95)

# Figure3
plot(gg_dta) +
  labs(y="Survival Probability", 
       x="Observation Time(years)",
       color="Treatment", 
       fill="treatment") +
  theme(legend.position=c(0.2,0.2)) +
  coord_cartesian(y=c(0,1.01))


plot(gg_dta, type = "cum_haz") +
  labs(y = "Cumulative Hazard",
       x = "Observation Time (years)",
       color = "Treatment", fill = "Treatment") +
  theme(legend.position = c(0.2, 0.8)) + 
  coord_cartesian(ylim = c(-0.02, 1.22))


#Trial set train (default nodesize is 15 for survival family - the paper used 3)

rfsrc_trial <- rfsrc(Surv(time,event) ~ .,
                   nodesize = 3,
                   data=trial,
                   nsplit=10, 
                   na.action="na.impute", 
                   tree.err=TRUE, 
                   importance=TRUE)

summary(rfsrc_pbc)

#Generalisation error
gg_error1<-gg_error(rfsrc_trial)
plot(na.omit(gg_error1))  

#Training set prediction 
#strCols should be defined "TRUE" for death(event), "FALSE" for censored
#strCol <- c("TRUE" = "red",
            "FALSE"="blue")

ggRFsrc <-plot(gg_rfsrc(rfsrc_trial),alpha=0.2)+
  theme(legend.position="none")+
  #scale_color_manual(values = strCol)+
  labs(y="Survival Probability",
       x="Time(years)")+
  coord_cartesian(ylim=c(-0.01,1.01))
show(ggRFsrc)

#make it more interpretable by comparing b/w treatment, groups showing as numbers not the treatment

plot(gg_rfsrc(rfsrc_trial, 
              by = "treatment")) +
  theme(legend.position = c(0.2, 0.2)) +
  labs(y = "Survival Probability", 
       x = "Time (years)") + 
  coord_cartesian(ylim = c(-0.01, 1.01))

#test set predictions

rfsrc_test <- predict(rfsrc_data2, 
                          newdata = test,
                          na.action = "na.impute",
                          importance = TRUE)

summary(rfsrc_test)
print(rfsrc_test)

plot(gg_rfsrc(rfsrc_test),alpha=0.2)+
#  scale_color_manual(values = strCol)+
  theme(legend.position="none")+
  labs(y="Survival Probability",
       x="Time(years)")+
  coord_cartesian(ylim=c(-0.01,1.01))


#variable importance(lbls=st.labs needs to be defined)

plot(gg_vimp(rfsrc_trial))+
  theme(legend.position=c(p=0.8,0.2))+
  labs(fill="VIMP > 0")

#minimal depth - nodesize different, threshold different

varsel_trial <- var.select(rfsrc_trial)

gg_md <- gg_minimal_depth(varsel_trial)
print(gg_md)
plot(gg_md)

#variable selection comparison

plot(gg_minimal_vimp(gg_md))+
  theme(legend.position = c(0.8,0.2))

#variable dependence

ggRFsrc + geom_vline(aes(xintercept=1), 
                     linetype="dashed")+
  coord_cartesian(xlim=c(0,5))


#plot(gg_v, 
    xvar=xvar[-1],
     panel=TRUE,
     alpha=0.4)+
  labs(y="Survival")+
  scale_color_manual(values = strCol)+
  theme(legend.position="none")+
  coord_cartesian(ylim=c(-0.05,1.05))

#box plot generated automatically if y values are factors(convert numeric into factors)

#plot(gg_v,
     xvar=xvar.cat,
     alpha=0.4)+
  labs(y="Survival")+
  scale_color_manual(values = strCol)+
  theme(legend.position="none")+
  coord_cartesian(ylim=c(-0.01,1.02))

#Partial dependence

#xvar <-c(xvar, xvar.cat)
#time_index <- c(which(rfsrc_pbc$time.interest>1)[1]-1,
                which(rfsrc_pbc$time.interest>3)[1]-1,
                which(rfsrc_pbc$time.interest>5)[1]-1)
#partial_pbc <- mclapply(rfsrc_pbc$time.interest[time_index],
                        function(tm){
                          plot.variable(rfsrc_pbc, surv.type = "surv",
                                        time=tm,
                                        xvar.names=xvar,
                                        partial=TRUE,
                                        show.plots = FALSE)
                        })


#gg_dta <-mclapply(partial_pbc, gg_partial)
#pbc_ggpart <- combine.gg_partial(gg_dta[[1]], 
                                 gg_dta[[2]],
                                 lbls=c("1 Year", "3 Years"))

#Figure 17


#plot(pbc_ggpart,
     panel = TRUE) +
  labs(y="Survival")+
  theme(legend.position="none")+
  coord_cartesian(ylim=c(0.43,1.0))


#Fiture 18

#ggplot(pbc_ggpart[["edema"]], aes(y=yhat, x=edema, col=group))+
  geom_boxplot(notch=TRUE,
               outlier.shape=NA) + # panel=TRUE,
  labs(x="Edema", y="Survival (%)",
       color="Time",
       shape="Time")+
  theme(legend.position=c(0.1,0.2))

#Variable interactions
#gg_interaction(rfsrc_pbc)

#plot(ggint, xvar=xvar)

#Conditional dependence plots

# Get variable dependence at 1 year 
#ggvar <- gg_variable(rfsrc_pbc, time = 1) 
# For labeling coplot membership
#ggvar$edema <- paste("edema = ", ggvar$edema, sep = "") 
# Plot with linear smooth (method argument)
#var_dep <- plot(ggvar, xvar = "bili",
                alpha = 0.5) +
  # geom_smooth(method = "glm",se = FALSE) +
  labs(y = "Survival",
       x = "bili") + 
  theme(legend.position = "none") + 
  coord_cartesian(y = c(-.01,1.01)) 

#var_dep
#var_dep+facet_grid(~edema)

