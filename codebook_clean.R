## IMPORT & SETUP ====

## for correlation matrices w/ significance stars
library(Hmisc)

## for detailed descriptives & Cronbach's alpha
library(psych)

## for lots of great shit!
## important to load last!
library(tidyverse)


## For Principal Components
library(FactoMineR)

## import csv. takes a few minutes (like 2 on current machine)
nels <- read_csv("nels_88_92_stmeg_v1_0.csv")
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



## rename them something more descriptive
## first create vector of desriptive names
iso_names <- c("popul", "athl", "social", "gdstud",
               "imptnt", "trble", "leader", "misfit")
## select by old names, apply new names
nels <- nels %>% 
  select(c(iso_vars)) %>% 
  `colnames<-`(c(iso_names))
## looks good!
describe(nels[, c(iso_names)])





## ISOLATION PRINCIPAL COMPONENTS ====

## GET CORRELATION MATRIX & P-VALUES
## using some subesetting methods that create new objects
## this may be more efficient for coding in the long run
## this is why you need to get smarter at coding efficiency!
iso_corrmat <- rcorr(as.matrix(nels[, c(iso_names)]), type = "pearson")
## return Pearson's r. consistent w/ stata
round(iso_corrmat$r, digits = 2)
## return p-values on test of difference from 0.
## all sig. except (trouble-maker, important)
round(iso_corrmat$P, digits = 3)



## CRONBACH'S ALPHAS ON WHOLE SET AND JUST ISOLATION SUBSET
## first, create isolation subset
iso_sub <- c("popul", "social", "imptnt", "leader")

## alpha on entire set - DOES NOT MATCH STATA!
iso_a.all <- psych::alpha(nels[, c(iso_names)])
iso_a.all$total[2]

## alpha excluding misfit - ALMOST matches stata (to 2 places).
## think I remember some exclusion in stata??
## doesn't natter b/c neither of these sets are used in PCA
iso_a.nomis <- psych::alpha(nels[, c("popul", "athl", "social", "gdstud",
                      "imptnt", "trble", "leader")])
iso_a.nomis$total[2]

## alpha on popularity subset. closely consistent with stata
iso_a.sub <- psych::alpha(nels[, c(iso_sub)])
iso_a.sub$total[2]


## COMPUTE PRINCIPAL COMPONENTS & LOADINGS



