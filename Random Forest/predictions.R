
# Obtaining predicted survival from rfsrc
# PRB, 17/06/19

library(ggRandomForests)

#
# load data and split in to train and test sets
#
data("pbc", package = "randomForestSRC")

pbc$years <- pbc$days/365
pbc$age <- pbc$age/365
pbc$treatment <- factor(pbc$treatment, 
                        levels = c(1,2), 
                        labels = c("DPCA", "placebo"), 
                        exclude = NA)
# get rid of days
pbc <- subset(pbc, select = -c(days))

pbc.trial <- pbc[!is.na(pbc$treatment),]
pbc.test <- pbc[is.na(pbc$treatment),]

#
# Train RF on the train set
#
rfsrc_pbc <- rfsrc(Surv(years, status) ~ ., 
                   data = pbc.trial,
                   nsplit = 10, 
                   nodesize = 3,
                   na.action = "na.impute",
                   tree.err = TRUE,
                   importance = TRUE)

#
# Get predictions on the test set
#
rfsrc_pbc_test <- predict(rfsrc_pbc, newdata = pbc.test,
                          na.action = "na.impute",
                          importance = TRUE)

#
# Plot predicted survival for all patients
#
plot(gg_rfsrc(rfsrc_pbc_test), alpha=.2) +
  theme(legend.position = "none") +
  labs(y = "Survival Probability", x = "Time (years)") +
  coord_cartesian(ylim = c(-0.01, 1.01))


#
# Choose a cut-off time
#
# Look at the vector of times which rfsrc produced predictions for
# Maybe choose the middle time
m <- round(length(rfsrc_pbc_test$time.interest)/2)
# Or closest to some chosen time
#t <- 3.827397
#m <- which.min(abs(rfsrc_pbc_test$time.interest - t))

t <- median(pbc.test)
m <- which.min(abs(rfsrc_pbc_test$time.interest - t))

t <- rfsrc_pbc_test$time.interest[m]

#
# Get the ground truth as a vector of who did survive beyond t
#
truth <- pbc.test$years > t

#
# Get the prediction of who rf thinks will survive > t
#
pred <- rfsrc_pbc_test$survival[,m] > 0.5

#
# Confusion matrix
#
cm <- table(truth, pred)

#
# Accuracy
#
accuracy <- (cm[1,1] + cm[2,2])/sum(cm)













