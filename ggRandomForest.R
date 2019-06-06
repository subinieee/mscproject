library(randomForestSRC)
library(dplyr)
library(ggplot2)
library(ggRandomForests)
library(parallel)
library(plot3D)
set.seed(100)
data("pbc",package="randomForestSRC")

#change "age","days" to "years"
pbc$days <- pbc$days/365
pbc$age <- pbc$age/365
colnames(pbc)[1] <- "years"

pbc$edema <- factor(pbc$edema, levels=c("0","0.5","1"), labels=c("0","0.5","1"))


#change treatment variable to "DPCA" and "Placebo"
pbc$treatment <- factor(pbc$treatment, levels= c("1","2"),labels=c("DPCA","Placebo"))

#data set variable dictionary

st.labs <- c("years" = "Time(years)", 
             "status" = "Event",
             "treatment" = "Treatment",
             "age" = "Age(years)",
             "sex"="Female=T",
             "ascites" = "Presence of Ascites",
             "hepatom" = "Presence of Hepatomegaly",
             "prothrombin" = "Prothrombin time (sec)",
             "spiders" = "Presence of Spiders",
             "edema" = "Edema",
             "bili" = "Serum Bilirubin (mg/dl)",
             "chol" = "Serum Choleserol (mg/dl)",
             "albumin" = "Albumin (mg/dl)",
             "copper" = "Urine Copper (ug/day)",
             "alk" = "Alkaline Phosphatase (U/litre)",
             "sgot" = "SGOT (U/ml)",
             "trig" = "Triglicerides (mg/dl)",
             "platelet" = "Platelets per cubic",
             "prothorombin" = "Prothrombin time (sec)",
             "stage" = "Histologic Stage")

#Create the trial and test data sets.
pbc.trial <- pbc %>% filter(!is.na(treatment))
pbc.test <- pbc %>% filter(is.na(treatment))

# Create the gg_survival object
gg_dta <- gg_survival(interval="years",
                      censor="status",
                      by="treatment",
                      data=pbc.trial,
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


pbc.bili <-pbc.trial
pbc.bili$bili_grp <-cut(pbc.bili$bili, 
                        breaks=c(0,0.8,1.3,3.4,29))

plot(gg_survival(interval="years",
                 censor="status",
                 by="bili_grp",
                 data=pbc.bili), error="none")+
  labs(y="Survival Probability", x="Observation Time (years)",
       color="Bilirubin")

#Trial set train(default nodesize is 15 for survival family - the paper used 3)

rfsrc_pbc <- rfsrc(Surv(years,status) ~ .,
                   nodesize = 3,
                   data=pbc.trial,
                   nsplit=10, 
                   na.action="na.impute", 
                   tree.err=TRUE, 
                   importance=TRUE)
#Generalisation error
summary(rfsrc_pbc)


gg_error1<-gg_error(rfsrc_pbc)
plot(na.omit(gg_error1))  


#Training set prediction 
#strCols should be defined "TRUE" for death(event), "FALSE" for censored

strCol <- c("TRUE" = "red",
          "FALSE"="blue")

ggRFsrc <-plot(gg_rfsrc(rfsrc_pbc),alpha=0.2)+
  theme(legend.position="none")+
  scale_color_manual(values = strCol)+
  labs(y="Survival Probability",
       x="Time(years)")+
  coord_cartesian(ylim=c(-0.01,1.01))
show(ggRFsrc)

#make it morer interpretable by comparing b/w treatment, groups showing as numbers not the treatment
plot(gg_rfsrc(rfsrc_pbc, 
              by = "treatment")) +
  theme(legend.position = c(0.2, 0.2)) +
  labs(y = "Survival Probability", 
       x = "Time (years)") + 
  coord_cartesian(ylim = c(-0.01, 1.01))

#test set predictions

rfsrc_pbc_test <- predict(rfsrc_pbc, 
                          newdata = pbc.test,
                          na.action = "na.impute",
                          importance = TRUE)

summary(rfsrc_pbc_test)
print(rfsrc_pbc_test)

plot(gg_rfsrc(rfsrc_pbc_test),alpha=0.2)+
  scale_color_manual(values = strCol)+
  theme(legend.position="none")+
  labs(y="Survival Probability",
       x="Time(years)")+
  coord_cartesian(ylim=c(-0.01,1.01))


#variable importance(lbls=st.labs needs to be defined)

plot(gg_vimp(rfsrc_pbc), lbls = st.labs)+
  theme(legend.position=c(p=0.8,0.2))+
  labs(fill="VIMP > 0")

#minimal depth - nodesize different, threshold different

varsel_pbc <- var.select(rfsrc_pbc)

gg_md <- gg_minimal_depth(varsel_pbc)
print(gg_md)
plot(gg_md)

#variable selection comparison

plot(gg_minimal_vimp(gg_md), lbls = st.labs)+
       theme(legend.position = c(0.8,0.2))

#variable dependence

ggRFsrc + geom_vline(aes(xintercept=1), 
                     linetype="dashed")+
  coord_cartesian(xlim=c(0,5))

gg_v <-gg_variable(rfsrc_pbc, time=c(1,3),
                   time.labels=c("1 Year", "3 Years"))

#scale_color_manual/shape_manual not defined - missing values for strCol, event.marks, event labels

plot(gg_v, xvar="bili", alpha=0.4)+#, se=FALSE
  labs(y="Survival", x="bili")+
  theme(legend.position = "none")+
  scale_color_manual(values = strCol)
  coord_cartesian(ylim=c(-0.01, 1.01))

xvar <- c("bili", "albumin", "copper", "prothrombin", "age")
xvar.cat<-c("edema")

plot(gg_v, 
     xvar=xvar[-1],
     panel=TRUE,
     alpha=0.4)+
  labs(y="Survival")+
  scale_color_manual(values = strCol)+
  theme(legend.position="none")+
  coord_cartesian(ylim=c(-0.05,1.05))

#box plot generated automatically if y values are factors(convert numeric into factors)

plot(gg_v,
     xvar=xvar.cat,
     alpha=0.4)+
  labs(y="Survival")+
  scale_color_manual(values = strCol)+
  theme(legend.position="none")+
  coord_cartesian(ylim=c(-0.01,1.02))

#Partial dependence

xvar <-c(xvar, xvar.cat)
time_index <- c(which(rfsrc_pbc$time.interest>1)[1]-1,
                which(rfsrc_pbc$time.interest>3)[1]-1,
                which(rfsrc_pbc$time.interest>5)[1]-1)
partial_pbc <- mclapply(rfsrc_pbc$time.interest[time_index],
                        function(tm){
                          plot.variable(rfsrc_pbc, surv.type = "surv",
                                        time=tm,
                                        xvar.names=xvar,
                                        partial=TRUE,
                                        show.plots = FALSE)
                        })


gg_dta <-mclapply(partial_pbc, gg_partial)
pbc_ggpart <- combine.gg_partial(gg_dta[[1]], 
                                 gg_dta[[2]],
                                 lbls=c("1 Year", "3 Years"))

#Figure 17


plot(pbc_ggpart,
     panel = TRUE) +
  labs(y="Survival")+
  theme(legend.position="none")+
  coord_cartesian(ylim=c(0.43,1.0))


#Fiture 18

ggplot(pbc_ggpart[["edema"]], aes(y=yhat, x=edema, col=group))+
  geom_boxplot(notch=TRUE,
               outlier.shape=NA) + # panel=TRUE,
  labs(x="Edema", y="Survival (%)",
       color="Time",
       shape="Time")+
  theme(legend.position=c(0.1,0.2))

#Variable interactions
gg_interaction(rfsrc_pbc)

plot(ggint, xvar=xvar)

#Conditional dependence plots

# Get variable dependence at 1 year 
ggvar <- gg_variable(rfsrc_pbc, time = 1) 
# For labeling coplot membership
ggvar$edema <- paste("edema = ", ggvar$edema, sep = "") 
# Plot with linear smooth (method argument)
var_dep <- plot(ggvar, xvar = "bili",
                alpha = 0.5) +
  # geom_smooth(method = "glm",se = FALSE) +
  labs(y = "Survival",
       x = "bili") + 
  theme(legend.position = "none") + 
  coord_cartesian(y = c(-.01,1.01)) 

var_dep
var_dep+facet_grid(~edema)

