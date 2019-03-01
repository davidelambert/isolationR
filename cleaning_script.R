## SETUP & IMPORT =========


library(tidyverse)

## csv version
nels8892 <- read_csv("nels_88_92_stmeg_v1_0.csv")
names(nels8892) <- tolower(names(nels8892)) ## lowercase columns  

## rdata version for comparison
load("NELS_88_92_STMEG_V1_0.rdata")
  stmeg <- STMEG  ## rename data data frame to lowercase
  rm(STMEG)  ## remove capitalized DF
  names(stmeg) <- tolower(names(stmeg))

## object size check - leave commented unless needed
## object.size(nels8892)
## object.size(stmeg)



## STANDARDIZED TEST SCORES =====

## get column numbers
## first, a vector of relevant names
irt_names <- c("by2xrstd", "by2xmstd", "by2xsstd", "by2xhstd", 
               "f12xrstd", "f12xmstd", "f12xsstd", "f12xhstd",
               "f22xrstd", "f22xmstd", "f22xsstd", "f22xhstd")
## return vector of column numbers
irt_nos <- match(irt_names, names(nels8892))

## future reference: can use names or numbers:
summary(nels8892[, c(irt_names)])
summary(nels8892[, c(irt_nos)])


## subset just the scores
scores_raw <- nels8892[, c(irt_names)]
summary(scores_raw)

## compare base year reading score w/ & w/o missing codes
## and count values from codebook
summary(scores_raw$by2xrstd)
summary(scores_raw$by2xrstd[scores_raw$by2xrstd < 99])
length(which(scores_raw$by2xrstd == 99.99))
length(which(scores_raw$by2xrstd == 99.98))

## this works to replace missing codes on A SINGLE VARIABLE
  ## scores_raw$by2xrstd[scores_raw$by2xrstd > 99] <- NA
  ## summary(scores_raw$by2xrstd)



## this works!
## loops over columns in the subsetted data frame
for (i in colnames(scores_raw)) {
  scores_raw[i][scores_raw[i] > 99] <- NA
}
summary(scores_raw)




## now try on whole dataset
  
  ## create testing frame & check out
  test <- nels8892
  summary(test[, c(irt_names)])
  
  
  ## now do the loop using the character vector of score names & check out
  for (i in colnames(test[, c(irt_names)])) {
    test[i][test[i] > 99] <- NA  
  }
  summary(test[, c(irt_names)])
  
  ## FUCK YEAH, that works! Here's a summary:
    ## 1. Create Vector of Column Names "irt_names"
    ## 2. 
  
  
  
  
