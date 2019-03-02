## IMPORT & SETUP ====

## for correlation matrices w/ significance stars
## library(Hmisc) prob can just do with corr()

## for detailed descriptives & Cronbach's alpha
library(psych)

## for lots of great shit!
## important to load last!
library(tidyverse)


## For Principal Components
library(FactoMineR)

## import csv. takes a few minutes (like 2 on current machine)
nels <- read_csv("data/nels_88_92_stmeg_v1_0.csv")
## give colums lowercase names
names(nels) <- tolower(names(nels))
## create backup for when you fuck up
backup <- nels




## TEST SCORE AVERAGES  ====
## standardized test score averages & dropout probability

## create list of each relevant test score column
irt_names <- c("by2xrstd", "by2xmstd", "by2xsstd", "by2xhstd", 
               "f12xrstd", "f12xmstd", "f12xsstd", "f12xhstd",
               "f22xrstd", "f22xmstd", "f22xsstd", "f22xhstd")

## summary of the subsetted variables, showing max of 99.99 on each
## n, mean & sd consistent w/ stata
describe(nels[, c(irt_names)])

## counts of the two missing codes from codebook
## counts are consistent w/ codebook
for (i in colnames(nels[, c(irt_names)])) {
  print(paste(colnames(nels[i]), ": ",
              "MISSING (99.98) = ",
                length(which(nels[i] == 99.98)), ", ",
              "TEST NOT COMP (99.99) = ",
                length(which(nels[i] == 99.99)),
              sep = ""
  ))
}


## loop over score columns, replace values over 99 with NA
for (i in colnames(nels[, c(irt_names)])) {
  nels[i][nels[i] > 99] <- NA  
}

## check results. still consistent w/ stata
describe(nels[, c(irt_names)])

## drop missings. still consistent w/ stata
nels <- nels %>% drop_na(c(irt_names))

## create unweighted averages
nels <- nels %>% 
  mutate(by_avg = ((by2xrstd + by2xmstd + by2xsstd + by2xmstd) / 4)) %>% 
  mutate(f1_avg = ((f12xrstd + f12xmstd + f12xsstd + f12xmstd) / 4)) %>% 
  mutate(f2_avg = ((f22xrstd + f22xmstd + f22xsstd + f22xmstd) / 4))


## summarize averaged variables.
## n is consistent w/ stata, but mean, sd, med, etc. not quite
## though not quite consistent w/ stata, still very close. rounding error?
avgs <- c("by_avg", "f1_avg", "f2_avg")
describe(nels[, c(avgs)])



## DROPOUT ====


## original vriable coded as numeric
summary(nels$f2univ2d)
class(nels$f2univ2d)

## add new column & convert to factor
nels <- nels %>% 
  ## after the below, only levels 1:3 remain, n's consistent w/ stata
  mutate(dropout = as.factor(f2univ2d)) %>% 
  ## below recodes and converts to a logical binary
  mutate(dropout = recode(dropout, `1` = FALSE, `2` = FALSE, `3` = TRUE))
## n's are consistent with stata
summary(nels$dropout)



## ISOLATION RECODING ====

## define list of popularity/isolation variables

iso_vars <- c("f1s67a", "f1s67b", "f1s67c", "f1s67d", 
               "f1s67e", "f1s67f", "f1s67g", "f1s67h")

## check example class: they're numeric
sapply(nels[, c(iso_vars)], class)

## get column numbers, just for memory (also works with names)
iso_nos <- match(iso_vars, names(nels))
iso_nos

## convert each to factor, using lapply
## for future  ref, lapply(argument to supply to function, funtion w/o ())
nels[, 801:808] <- lapply(nels[, 801:808], as.factor)

## check to make sure - yep!
sapply(nels[, c(iso_vars)], class)

## get counts to match to codebook & stata - looks good!
sapply(nels[, c(iso_vars)], summary)


## replace with lapply rather than FOR loop as above
## for future reference: the FUNCTION argument of lapply
## gets defined as a new function.
## the first argument, the list of column names becomes x in replace(),
## function(x) used when the first argument gets used more than once
nels[, c(iso_vars)] <- lapply(nels[, c(iso_vars)],
                              function(x) replace(x, x %in% 6:8, NA))

## check results 
sapply(nels[, c(iso_vars)], summary)

## looks good, but need to remove extra levels
nels[, c(iso_vars)] <- lapply(nels[, c(iso_vars)], droplevels)
sapply(nels[, c(iso_vars)], summary)

## drop the missings & recheck both frequencies and means
## consistent with stata
nels <- nels %>% drop_na(c(iso_vars))
sapply(nels[, c(iso_vars)], summary)

## convert back to numeric to use in PCA
nels[, c(iso_vars)] <- lapply(nels[, c(iso_vars)], as.numeric)
## check against stata as continuous - looks good!
describe(nels[, c(iso_vars)])



## make new columns with descriptive names (preserving orgininals)
nels <- nels %>% 
  mutate(popul = f1s67a) %>% 
  mutate(athl = f1s67b) %>% 
  mutate(social = f1s67c) %>% 
  mutate(gdstud = f1s67d) %>% 
  mutate(imptnt = f1s67e) %>% 
  mutate(trbl = f1s67f) %>% 
  mutate(leadgrp = f1s67g) %>% 
  mutate(nogrp = f1s67h)


## create vector of desriptive names
iso_names <- c("popul", "athl", "social", "gdstud",
               "imptnt", "trbl", "leadgrp", "nogrp")
## looks good!
describe(nels[, c(iso_names)])





## ISOLATION PRINCIPAL COMPONENTS ====

## GET CORRELATION MATRIX & P-VALUES

  # ## THIS USES THE Hmisc package & isn't necessary
  #   ## using some subesetting methods that create new objects
  #   ## this may be more efficient for coding in the long run
  #   ## this is why you need to get smarter at coding efficiency!
  #   iso_corrmat <- rcorr(as.matrix(nels[, c(iso_names)]), type = "pearson")
  #   ## return Pearson's r. consistent w/ stata
  #   round(iso_corrmat$r, digits = 2)
  #   ## return p-values on test of difference from 0.
  #   ## all sig. except (trouble-maker, important)
  #   round(iso_corrmat$P, digits = 3)

## Correlation matrix
round(cor(nels[, c(iso_names)]), 3)



## CRONBACH'S ALPHAS ON WHOLE SET AND JUST ISOLATION SUBSET

## first, create isolation subset
iso_sub <- c("popul", "social", "imptnt", "leadgrp")

## alpha on entire set - DOES NOT MATCH STATA!
iso_a.all <- psych::alpha(nels[, c(iso_names)])
iso_a.all$total[2]

## alpha excluding misfit - ALMOST matches stata (to 2 places).
## think I remember some exclusion in stata??
## doesn't natter b/c neither of these sets are used in PCA
iso_a.nomis <- psych::alpha(nels[, c("popul", "athl", "social", "gdstud",
                      "imptnt", "trbl", "leadgrp")])
iso_a.nomis$total[2]

## alpha on popularity subset. closely consistent with stata
iso_a.sub <- psych::alpha(nels[, c(iso_sub)])
iso_a.sub$total[2]



## COMPUTE PRINCIPAL COMPONENTS & LOADINGS ====

## create subset for efficiency
iso_only <- nels %>% 
  select(stu_id, c(iso_sub))

## convert stu_id to rownnames for matching later
## need a data.frame, not a tibble
iso_only <- as.data.frame(iso_only)
## assign rownames
rownames(iso_only) <- iso_only[, 1]
## drop stu_id column
iso_only <- iso_only[,-1]
## loks good!
head(iso_only)



## compute PCA scores

## using psych::principal
iso_pca <- principal(iso_only, rotate = "varimax", scores = TRUE)
## consistent w/ stata! only 1 factor retained, just like stata
head(iso_pca4$scores)
## further stata checks:
  iso_pca$values ## eigenvalues - good!
  iso_pca$loadings ## loadings - good!
  1 - iso_pca$communality ## uniqueness - good!
    
    # ## ALL THE METHODS BELOW DO *NOT* PROVIDE *ROTATED* LOADINGS AND SCORES
    # ## I DON'T REMEMBER WHY ROTATING WAS IMPORTANT, BUT THAT'S WHAT WE DID
    # ## IN STATA. THERE ARE OTHER WAYS TO GET THEM, PROVIDED HERE:
    # ## https://stats.stackexchange.com/questions/59213/
    # ## how-to-compute-varimax-rotated-principal-components-in-r
    # ## PRESERVED FOR REFERENCE ONLY.
    #   ## Using PCA() from FactoMineR
    #   iso_pca1 <- PCA(iso_only, graph = FALSE)
    #     
    #     ## get eigenvalues. consistent w/ stata!
    #     iso_pca1$eig
    #     
    #     ## get correlations. also consistent with stata!
    #     iso_pca1$var$coord
    #     
    #   
    #   ## Using prcomp() from base
    #   iso_pca2 <- prcomp(iso_only, scale. = TRUE, retx = TRUE)
    #   
    #     ## get eigenvalues by squaring the sdev vector
    #     ## matches PCA() results and stata
    #     ## method from: https://stat.ethz.ch/pipermail/r-help/2005-August/076610.html
    #     iso_pca2$sdev^2
    #     
    #     
    #     ## get predicted vars
    #     iso_pred2 <-  data.frame(predict(iso_pca2))
    #     head(iso_pred2)
    #     
    #     ## get ROTATION for prediction
    #     
    #     
    #   ## Using princomp()
    #   iso_pca3 <- princomp(iso_only, scores = TRUE, cor = TRUE)
    #   summary(iso_pca3)
    #   iso_pca3$sdev^2
    #   
    #   loadings(iso_pca3)
    #   
    #   head(iso_pca3$scores)



