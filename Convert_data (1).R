

#Convert the dataset into .dat and .names file as required by SaddlePoint Signature software.

#Load the data

library(survival)
data(ovarian)

filename <- "ovarian"

# Copy data reordering the cols
d <- subset(ovarian, select = c(age, resid.ds, rx, ecog.ps))

d$time <- ovarian$futime
d$status <- ovarian$fustat


N <- dim(d)[1]     # samples
P <- dim(d)[2]-2   # covariates excluding outcome data
R <- 2             # event types

# create file and write header
sink(paste0(filename, ".dat"))
  cat(paste0("N=", N, "\n"))
  cat(paste0("P=", P, "\n"))
  cat(paste0("R=", R, "\n"))
  sink()
  
# append data
write.table(d, file = paste0(filename, ".dat"), append = T, sep = " ", row.names = F, col.names = F)


# write a subset of names given the vector of TRUEs and FALSEs.
write.table(names(d)[c(rep(T,P),F,F,F,F)], file = paste0(filename, ".names"), 
            quote = F, sep = " ", row.names = T, col.names = F)


# write a subset of names given the vector of TRUEs and FALSEs.
# First F excludes ID, then P times T for the covariates, then some Fs to exclude the event data
# remember the vector wil be recycled, this guarentees P names without hard coding
#write.table(names(d)[c(F,rep(T,P),F,F,F,F)], file = paste0(filename, ".names"), 
#            quote = F, sep = " ", row.names = T, col.names = F)


