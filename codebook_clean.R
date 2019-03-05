## IMPORT & SETUP ====

## for correlation matrices w/ significance stars
## library(Hmisc) prob can just do with cor()

## for regression anakysis, mostly from pkg:car
## also, just good practice
library(AER)

## for PCA & detailed descriptives
library(psych)

## for lostd of shit - important to load last
library(tidyverse)




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
## freq's are consistent with stata
summary(nels$dropout)



## ISOLATION PCA CLEANING ====

## define list of popularity/isolation variables
  ## match
  iso_vars <- grep("^f1s67", colnames(nels))
  ## get names instead of numbers
  iso_vars <- colnames(nels[, c(iso_vars)])

  ## old version
  ## iso_vars <- c("f1s67a", "f1s67b", "f1s67c", "f1s67d", 
  ##              "f1s67e", "f1s67f", "f1s67g", "f1s67h")

## check example class: they're numeric
sapply(nels[, c(iso_vars)], class)


## convert each to factor, using lapply
## for future  ref, lapply(argument to supply to function, funtion w/o ())
nels[, c(iso_vars)] <- lapply(nels[, c(iso_vars)], as.factor)

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





## ISOLATION PCA PRELIMS ====

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
## round(cor(nels[, c(iso_names)]), 3)

## Better: lower triangle only & no rounding necessary
lowerCor(nels[, c(iso_names)])



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
head(iso_pca$scores)
## further stata checks:
  iso_pca$values ## eigenvalues - good!
  iso_pca$loadings ## loadings - good!
  1 - iso_pca$communality ## uniqueness - good!
  

## convert scores to a d.f. w/ only stu_id & PCA score
iso_scores <- as.data.frame(iso_pca$scores)
iso_scores <- iso_scores %>% 
  ## rownames back to column
  rownames_to_column(var = "stu_id") %>%
  ## rename PC1 variable
  mutate(isolation = PC1) %>% 
  ## drop rown number column
  select(stu_id, isolation)
  
## match to main d.f.
nels <- nels %>% 
  merge(iso_scores, by = "stu_id", sort = FALSE)
## looks good!
head(nels[, c(1, 6831:6843)])


## PCA REJECTS ====
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





## SELF-CONCEPT ========

## check number of missings codded as 99.98 - matches stata
sum(as.logical(which(nels$f1cncpt2 == 99.98)))
## replace w/ NA
nels$f1cncpt2[nels$f1cncpt2 == 99.98] <- NA
  ## make sure that worked - good!
  sum(is.na(nels$f1cncpt2))
## drop missings
nels <- nels %>% drop_na(f1cncpt2)
  ## make sure that worked - good!
  sum(is.na(nels$f1cncpt2))
## rename
nels <- nels %>% mutate(selfcncpt = f1cncpt2)



## DEPRESSION PCA CLEANING ====

## get numbers of columns in psych subset - yiels 742:755
psych_vars <- grep("^f1s62", colnames(nels))
psych_vars <- colnames(nels[, c(psych_vars)])

## convert to factor:
nels[, c(psych_vars)] <- lapply(nels[, c(psych_vars)], as.factor)

## replace missing codes w/ NA
nels[, c(psych_vars)] <- lapply(nels[, c(psych_vars)],
                              function(x) replace(x, x %in% 6:8, NA))

## drop unused 6 & 8 levels
nels[, c(psych_vars)] <- lapply(nels[, c(psych_vars)], droplevels)

## drop missings - N matches stata!
nels <- nels %>% drop_na(c(psych_vars))



## subset & rename
depr_only <- nels %>% 
  select(stu_id, f1s62a, f1s62d, f1s62e, f1s62h, 
         f1s62i, f1s62j, f1s62l, f1s62n) %>% 
  `colnames<-`(c("stu_id", "regard", "worth", "ability", "satis",
               "useless", "nogood", "proud", "empty"))


## reverse coding on poitively coded/negatively worded questions
## makes incresing scores indicate increasingly negative symptoms

  ## check original by frequency and by mean
  ## consistent w/ stata on freq.
  ## means of on last 4 b/c not reversed yet.
  sapply(depr_only[,-1], summary)
  sapply(depr_only[,-1], function(x) summary(as.numeric(x)))

  
  ## reverse last 4
  depr_only[, 6:9] <- lapply(depr_only[, 6:9], 
                             function(x) car::recode(x, "1 = 4; 2 = 3; 
                                                     3 = 2; 4 = 1"))
  
  ## it works!
  sapply(depr_only[,-1], summary)
  sapply(depr_only[,-1], function(x) summary(as.numeric(x)))

## convert back to numeric to use in PCA
depr_only[, 2:9] <- lapply(depr_only[, 2:9], as.numeric)






## DEPRESSION PCA ====

## alpha on all except "emotionally empty" - close to stata
depr_a.noemp <- psych::alpha(depr_only[, 2:8])
depr_a.noemp$total[2]

## alpha on all - also close to stata
depr_a.all <- psych::alpha(depr_only[, 2:9])
depr_a.all$total[2]


## convert from tibble to d.f.
depr_only <- as.data.frame(depr_only)

## assign rownames
rownames(depr_only) <- depr_only[, 1]

## delete stu_id column
depr_only <- depr_only[, -1]

## conduct PCA - now matches stata (or at least very very close)
depr_pca <- psych::principal(depr_only, rotate = "varimax", nfactors = 2, scores = TRUE)
head(depr_pca$scores)

## supplemental analysis
depr_pca$values  ## now matches stata
depr_pca$loadings  ## now close to stata, but not exact
1 - depr_pca$communality ## now matches stata


## scores to data frame
depr_scores <- as.data.frame(depr_pca$scores)

## rownames back to stu_id column
depr_scores <- depr_scores %>% 
  rownames_to_column(var = "stu_id") %>% 
  mutate(depression = RC1) %>% ## keep only 1st component, as in stata
  select(stu_id, depression)

## merge back into main dataset
nels <- nels %>% 
  merge(depr_scores, by = "stu_id", sort = FALSE)

## looks good, given weird mismatch w/stata
head(nels[, c(1, 6831:6834, 6843:6845)])




## DEPRESSION FACTOR ANALYSIS =====

## subset & rename
depr2 <- nels %>% 
  select(stu_id, c(psych_vars)) %>% 
  `colnames<-`(c("stu_id", "feelgood", "noctrl", "luckimpt1", "worth",
                 "ability", "obstrd", "planfail", "selfsat", "useless",
                 "nogood", "planwork", "nopride", "luckimpt2", "empty"))


  ## check original by frequency and by mean
  ## consistent w/ stata on freq.
  ## means of on last 4 b/c not reversed yet.
  sapply(depr2[,-1], summary)
  sapply(depr2[,-1], function(x) mean(as.numeric(x)))

  
  ## reverse coding on positive coding/negative wording
  ## means all increasing scores are increasingly bad
  depr2[, c(3:4, 7:8, 10:11, 13:15)] <- lapply(
     depr2[, c(3:4, 7:8, 10:11, 13:15)], function(x) car::recode(
                                                      x, "1 = 4; 2 = 3; 3 = 2; 4 = 1"))
  
  ## looks good - matches stata
  sapply(depr2[,-1], summary)
  sapply(depr2[,-1], function(x) mean(as.numeric(x)))

## convert back to numeric to use in PCA
depr2[, 2:15] <- lapply(depr2[, 2:15], as.numeric)


## overall alpha
depr2_a.all <- psych::alpha(depr2[, 2:15])
depr2_a.all$total[2]
## HIGHER than the condensed ones!
depr_a.all$total[2]


## conduct factor alaysis
depr2_fa <- psych::fa(depr2[, 2:15], rotate = "varimax")
depr2_fa$values
depr2_fa$loadings
## kind low uniqueness, but roll w/ it
depr2_fa$uniquenesses ## sysnonym for: 1 - depr2_fa$communality

## predictions
depr2_scores <- as.data.frame(predict.psych(depr2_fa, depr2[, 2:15]))
## right number of obs, but no ids - do right join later
head(depr2_scores)



## secondary factor analysis using only same vars as PCA
## explains higher proportion of variance (40% vs 31%)
## still pretty low, but use in modelling
depr3_fa <- fa(depr2[, c(2,5,6,9,10,11,13,15)], rotate = "varimax")
depr3_fa$values
depr3_fa$loadings
depr3_fa$uniquenesses

## predictions for restricted model
depr3_scores <- as.data.frame(predict.psych(depr3_fa, 
                                            depr2[, c(2,5,6,9,10,11,13,15)]))

## join prediction columns & drop individual variables
depr2$depr2 <- depr2_scores$MR1
depr2$depr3 <- depr3_scores$MR1
depr2 <- depr2[, c(1, 16:17)]

## merge into main dataset
nels <- merge(nels, depr2, by = "stu_id", sort = FALSE)
head(nels[, c(1, 6831:6834, 6843:6847)])


## COVARIATE CLEANING =====

## dummy on male sex
  class(nels$sex) ## currently numeric, 1 = male, 2 = female
  ## drop missings - should drop any, but just for good measure
  nels <- nels %>% drop_na(sex)
  nels$male <- ifelse(nels$sex == 1, nels$male <- TRUE, nels$male <- FALSE)
  ## matches stata
  summary(nels$male)


## 10th grade composite SES
  ## recode numbered missings (there are none - for good measure)
  nels$f1ses[nels$f1ses == 99.998] <- NA
  ## drop missings (there are none - for good measure)
  nels <- nels %>% drop_na(f1ses)
  ## matches stata
  summary(nels$f1ses)
  

## Single-category race
  ## convert to factor & count
  nels$race_bu <- nels$race
  nels$race <- as.factor(nels$race)
  summary(nels$race) ## matches stata
  ## descriptive names. can't do 8 = NA for some reason
  nels$race <- recode(nels$race, `1` = "asian", `2` = "hispanic",
                      `3` = "black", `4` = "white", `5` = "natam", `8`= "missing")
  ## convert "missing" to NA
  nels$race[nels$race == "missing"] <- NA
  ## drop "missing" level
  nels$race <- droplevels(nels$race)
  ## drop NA's
  nels <- nels %>% drop_na(race)
  summary(nels$race) ## matches stata

## dummy on nonwhite status
  nels$nonwhite <- ifelse(nels$race != "white", nels$nonwhite <- TRUE, 
                          nels$nonwhite <- FALSE)
  summary(nels$nonwhite)  ## matches stata
  
## dummy on hispanic status
  nels$hispanic <- ifelse(nels$race == "hispanic", nels$hispanic <- TRUE, 
                          nels$hispanic <- FALSE)
  summary(nels$hispanic)

## dummy on 1st language spanish speaker
  ## first convert original language variable to factor
  nels$f1n12 <- as.factor(nels$f1n12)
  summary(nels$f1n12) ## matches stata
  ## convert missing code 98 to NA
  nels$f1n12[nels$f1n12 == 98] <- NA
  ## drop level 98
  nels$f1n12 <- droplevels(nels$f1n12)
  ## drop missings
  nels <- nels %>% drop_na(f1n12)
  ## create spanish language dummy
  nels$spanish <- ifelse(nels$f1n12 == 2, nels$spanish <- TRUE, 
                         nels$spanish <- FALSE)
  summary(nels$spanish) ## looks good!
  
## school percentage white
  ## convert original to factor
  nels$f1c27f <- as.factor(nels$f1c27f)
  summary(nels$f1c27f) ## matches stata
  ## convert missing/refuse codes to NA
  nels$f1c27f[nels$f1c27f == 997] <- NA
  nels$f1c27f[nels$f1c27f == 998] <- NA
  ## drop those levels  
  nels$f1c27f <- droplevels(nels$f1c27f)
  ## drop missings
  nels <- nels %>% drop_na(f1c27f)
  summary(nels$f1c27f) ## matches stata
  ## new variable w/ desciptive name
  nels$pctwht <- nels$f1c27f
  ## rename levels & make sure its ordered
  nels$pctwht <- recode_factor(nels$pctwht, `1` = "0-25", `2` = "26-50",
                               `3` = "51-75", `4` = "76-90", `5` = "91-100",
                               .ordered = TRUE)
  summary(nels$pctwht) ## looks good!
  
## dummy on school is majority white
  nels$majwht <- ifelse(nels$pctwht == "0-25" | nels$pctwht == "26-50",
                        nels$majwht <- FALSE, nels$majwht <- TRUE)
  summary(nels$majwht) ## matches stata

## interaction dummy between hispanic student & majority white school
  nels <- nels %>% 
    mutate(hispwht = hispanic * majwht)
  ## convert to logical
  nels$hispwht <- as.logical(nels$hispwht)
  summary(nels$hispwht) ## matches stata
  
  
  
## FINAL SUBSET & OUTPUT ====
  ## reminder
  head(nels[, c(1, 6831:6834, 6843:6854)])
  
  ## subset
  small <- nels %>% 
    select(stu_id, f1sch_id, by_avg, f1_avg, f2_avg, dropout, isolation,
           selfcncpt, depression, depr2, depr3, male, race, nonwhite,
           hispanic, spanish, pctwht, majwht, hispwht)
  
  ## write out
  write.csv(small, file = "nels_small.csv", row.names = FALSE)
  