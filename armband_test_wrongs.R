#!/usr/bin/env Rscript
setwd("/opt/BA/Evaluation")
source('load_results.R')

armband_wrongs_0 = list.map(results, test_results$armband_test_wrongs_0)
armband_wrongs_1 = list.map(results, test_results$armband_test_wrongs_1)

pdf(file="plots/armband_wrongs_0.pdf", width = 5)
hist(unlist(armband_wrongs_0), breaks = seq(-0.5, 19, 1), xlab = "Number of errors in initial wrist band test", main = "Initial recognition test before main study")
dev.off()

pdf(file="plots/armband_wrongs_1.pdf", width = 5)
hist(unlist(armband_wrongs_1), breaks = seq(-0.5, 6, 1), xlab = "Number of errors in initial wrist band test", main = "Initial recognition test before retest")
dev.off()

sort(unlist(armband_wrongs_1))
