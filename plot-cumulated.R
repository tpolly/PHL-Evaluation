#!/usr/bin/env Rscript
setwd("/opt/BA/Results")
source('load_results.R')

# TODO: use rlist to filter nulls in pattern_distance or say that null means max distance = length
# and decide how to deal with the first 4 subsets not containing numbers 5-9

pdf(file = 'construct_test_pattern_distance.pdf')
plot(range(1,13), range(0,5), type="n", xlab = "Subset", ylab = "Pattern distance")
for (list in results) {
  lines(unlist(map(list$test_results$construct, function(subset) {mean(unlist(map(subset, 'distance')))})), col = list$color)
}
dev.off()

pdf(file = 'construct_test_pattern_correct.pdf')
plot(range(1,13), range(0,1), type="n", xlab = "Subset", ylab = "Correct")
for (list in results) {
  lines(unlist(map(list$test_results$construct, function(subset) {mean(unlist(map(subset, 'correct')))})), col = list$color)
}
dev.off()

pdf(file = 'construct_test_time_elapsed.pdf')
plot(range(1,13), range(0,10), type="n", xlab = "Subset", ylab = "Time elapsed")
for (list in results) {
  lines(unlist(map(list$test_results$construct, function(subset) {mean(unlist(map(subset, 'time_elapsed')))})), col = list$color)
}
dev.off()



pdf(file = 'recognize_test_pattern_distance.pdf')
plot(range(1,13), range(0,5), type="n", xlab = "Subset", ylab = "Pattern distance")
for (list in results) {
  lines(unlist(map(list$test_results$recognize, function(subset) {mean(unlist(map(subset, 'pattern_distance')))})), col = list$color)
}
dev.off()

pdf(file = 'recognize_test_pattern_correct.pdf')
plot(range(1,13), range(0,1), type="n", xlab = "Subset", ylab = "Correct")
for (list in results) {
  lines(unlist(map(list$test_results$recognize, function(subset) {mean(unlist(map(subset, 'correct')))})), col = list$color)
}
dev.off()

pdf(file = 'recognize_test_time_elapsed.pdf')
plot(range(1,13), range(0,10), type="n", xlab = "Subset", ylab = "Time elapsed")
for (list in results) {
  lines(unlist(map(list$test_results$recognize, function(subset) {mean(unlist(map(subset, 'time_elapsed')), trim = 0.2)})), col = list$color)
}
dev.off()

# todo: try plotting confidence bands, example like this:
# df <- data.frame(x =1:10,
#                  F =runif(10,1,2),
#                  L =runif(10,0,1),
#                  U =runif(10,2,3))
# 
# 
# plot(df$x, df$F, ylim = c(0,4), type = "l")
# #make polygon where coordinates start with lower limit and 
# # then upper limit in reverse order
# polygon(c(df$x,rev(df$x)),c(df$L,rev(df$U)),col = "grey75", border = FALSE)
# lines(df$x, df$F, lwd = 2)
# #add red lines on borders of polygon
# lines(df$x, df$U, col="red",lty=2)
# lines(df$x, df$L, col="red",lty=2)