## SETUP & IMPORT ====

## libraries
library(psych)
library(AER) ## not needed?
library(lfe) ## fixed effects
library(stargazer) ## tables
library(tidyverse)

## import cleaned & abbreviated dataset
nels <- read.csv("nels_small.csv")




## MODELLING ====

## convert race into dummies NOTE: ALREADY HAVE hispanic
nels$asian <- ifelse(nels$race == "asian", 
                     nels$asian <- TRUE, nels$asian <- FALSE)
nels$black <- ifelse(nels$race == "black",
                     nels$black <- TRUE, nels$black <- FALSE)
nels$natam <- ifelse(nels$race == "natam",
                     nels$natam <- TRUE, nels$natam <- FALSE)


## basic models
## slightly different estimates from stata du to diff test averages
test_basic <- lm(f1_avg ~ isolation, data = nels)
summary(test_basic)
dropout_basic <- lm(dropout ~ isolation, data = nels)
summary(dropout_basic)


## multiple regression models
test_controls <- lm(f1_avg ~ isolation + by_avg + f1ses + male +
                    black + hispanic + asian + natam, data = nels)
summary(test_controls)
dropout_controls <- lm(dropout ~ isolation + f1_avg + f1ses + male +
                    black + hispanic + asian + natam, data = nels)
summary(dropout_controls)


## fixed effects models
test_fe <- felm(f1_avg ~ isolation + by_avg + f1ses + male +
                black + hispanic + asian + natam | f1sch_id, data = nels)
summary(test_fe)
dropout_fe <- felm(dropout ~ isolation + f1_avg + f1ses + male +
                   black + hispanic + asian + natam | f1sch_id, data = nels)
summary(dropout_fe)


## self-concept fixed effects
## quite different results from stata! - but different sample
test_self <- felm(f1_avg ~ selfcncpt + by_avg + f1ses + male +
                    black + hispanic + asian + natam | f1sch_id, data = nels)
summary(test_self)
dropout_self <- felm(dropout ~ selfcncpt + f1_avg + f1ses + male +
                     black + hispanic + asian + natam | f1sch_id, data = nels)
summary(dropout_self)


## depression (main) fixed effects
## quite different results from stata! - but different sample
test_depr <- felm(f1_avg ~ depression + by_avg + f1ses + male +
                    black + hispanic + asian + natam | f1sch_id, data = nels)
summary(test_depr)
dropout_depr <- felm(dropout ~ depression + f1_avg + f1ses + male +
                     black + hispanic + asian + natam | f1sch_id, data = nels)
summary(dropout_depr)



## RESULTS TABLES ====

## Main results
stargazer(test_basic, test_controls, test_fe,
          dropout_basic, dropout_controls, dropout_fe,
          type = "text", align = TRUE, digits = 3,
          title = "Primary Models",
          model.names = FALSE,
          dep.var.caption = "Outcome:",
          dep.var.labels = c("10th Grade Test Average",
                             "Dropout by 12th Grade"),
          keep.stat = c("n"), omit = "Constant",
          add.lines = list(c("School Fixed Effects", "N","N","Y","N","N","Y")),
          covariate.labels = c("Isolation Score",
                               "8th Gr. Test Avg.",
                               "10th Gr. Test Avg.",
                               "Family SES",
                               "Male", "Black", "Hispanic",
                               "Asian", "Native American"),
          notes.align = "l"
          )


## Alternate Predictors
stargazer(test_fe, test_self, test_depr,
          dropout_fe, dropout_self, dropout_depr,
          type = "text", align = TRUE, digits = 3,
          title = "Alternate Predictors",
          model.names = FALSE,
          dep.var.caption = "Outcome:",
          dep.var.labels = c("10th Grade Test Average",
                             "Dropout by 12th Grade"),
          keep.stat = c("n"),
          omit = c("Constant", "by_avg", "f1_avg", "f1ses", "male",
                   "black", "hispanic", "asian", "natam"),
          add.lines = list(c("Controls", "N","Y","Y","N","Y","Y"),
                           c("School Fixed Effects", "N","N","Y","N","N","Y")),
          notes.align = "l",
          notes = "Controls: prior test average, compsite SES, sex, and race"
          )
