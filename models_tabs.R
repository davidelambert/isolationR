## SETUP & IMPORT ====

## libraries
library(psych)
library(AER)
library(lfe)
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
