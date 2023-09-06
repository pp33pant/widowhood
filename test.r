rm(list = ls())
library(tidyverse)
library(survival)
library(survminer)
library(haven)
library(xgboost)
library(pROC)
library(caret)
library(BART)
library(naniar)
library(ggplot2)

source("source.R")
set.seed(1234)

male<- read_dta("male.dta")
female<- read_dta("female.dta")


# Main Effect 
plot1 <- dress_plot(hte_ate_model(male, "health", "xgboost", "health"), "Men") 
plot2 <- dress_plot(hte_ate_model(female, "health", "xgboost", "health"), "Women")

ggarrange(plot1, plot2, common.legend = T, legend = "bottom")

# Age Differences 

# test the flexible age cuts
edu_status()