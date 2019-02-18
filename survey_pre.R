#!/usr/bin/env Rscript
setwd("/opt/BA/Evaluation")
source('load_results.R')

age_all <- map(results, function(list) {2018 - list$survey_pre_results$birth_year})

age_high <- map(results_high, function(list) {2018 - list$survey_pre_results$birth_year})
age_high_fb <- map(results_high_with_feedback, function(list) {2018 - list$survey_pre_results$birth_year})
age_low <- map(results_low, function(list) {2018 - list$survey_pre_results$birth_year})
age_low_fb <- map(results_low_with_feedback, function(list) {2018 - list$survey_pre_results$birth_year})
age_no <- map(results_no, function(list) {2018 - list$survey_pre_results$birth_year})

pdf(file = 'plots/age_distribution.pdf', width = 5, height = 3)
hist(unlist(age_all), main = "Study participant age distribution", xlab="Age", ylab="Number of participants", xlim = range(10,60))
dev.off()

pdf(file = 'plots/high_age_distribution.pdf', width = 5, height = 3)
hist(unlist(age_high), main = "Study participant age distribution", xlab="Age", ylab="Number of participants", xlim = range(10,60))
dev.off()

pdf(file = 'plots/high_with_feedback_age_distribution.pdf', width = 5, height = 3)
hist(unlist(age_high_fb), main = "Study participant age distribution", xlab="Age", ylab="Number of participants", xlim = range(10,60))
dev.off()

pdf(file = 'plots/low_age_distribution.pdf', width = 5, height = 3)
hist(unlist(age_low), main = "Study participant age distribution", xlab="Age", ylab="Number of participants", xlim = range(10,60))
dev.off()

pdf(file = 'plots/low_with_feedback_age_distribution.pdf', width = 5, height = 3)
hist(unlist(age_low_fb), main = "Study participant age distribution", xlab="Age", ylab="Number of participants", xlim = range(10,60))
dev.off()

pdf(file = 'plots/no_age_distribution.pdf', width = 5, height = 3)
hist(unlist(age_no), main = "Study participant age distribution", xlab="Age", ylab="Number of participants", xlim = range(10,60))
dev.off()