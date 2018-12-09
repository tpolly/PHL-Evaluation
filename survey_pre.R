#!/usr/bin/env Rscript
setwd("/opt/BA/Evaluation")
source('load_results.R')
library(vioplot)

age <- map(results, function(list) {2018 - list$survey_pre_results$birth_year})
age_high <- map(results_high, function(list) {2018 - list$survey_pre_results$birth_year})
age_low <- map(results_low, function(list) {2018 - list$survey_pre_results$birth_year})
age_no <- map(results_no, function(list) {2018 - list$survey_pre_results$birth_year})

pdf(file = 'age_distribution.pdf', width = 5, height = 3)
hist(unlist(age), main = "Study participant age distribution", xlab="Age", ylab="Number of participants", xlim = range(10,60))
dev.off()