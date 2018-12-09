#!/usr/bin/env Rscript
setwd("/opt/BA/Evaluation")
source('load_results.R')

phl_df_plot <- function(df, xmin, xmax, ymin, ymax, lightcolor, darkcolor, xlab, ylab, xleg, yleg, main) {
  plot(range(xmin, xmax), range(ymin, ymax), type = "n", xlab = xlab, ylab = ylab, main = main)
  #apply(df, 1, function(row) {lines(row, col="gray90")})
  polygon(c(xmin:xmax,xmax:xmin),c(apply(df, 2, quantile, probs = 0.25), rev(apply(df, 2, quantile, probs = 0.75))), col = lightcolor, border = FALSE)
  lines(apply(df, 2, median), col=darkcolor, lty=5)
  lines(apply(df, 2, mean), col=darkcolor, lty = 1)
  lines(apply(df, 2, quantile, probs = 0.25), col=darkcolor, lty = 2)
  lines(apply(df, 2, quantile, probs = 0.75), col=darkcolor, lty = 2)
  lines(apply(df, 2, min), col=darkcolor, lty=3)
  lines(apply(df, 2, max), col=darkcolor, lty=3)
  legend(xleg, yleg, legend=c("Mean", "Median", "Upper/lower quartile", "Min/max"), col=c(darkcolor, darkcolor, darkcolor, darkcolor), lty=c(1,5,2,3), cex=0.8)
}

phl_df_full_plot <- function(df, xmin, xmax, ymin, ymax, lightcolor, darkcolor, xlab, ylab, xleg, yleg, main) {
  plot(range(xmin, xmax), range(ymin, ymax), type = "n", xlab = xlab, ylab = ylab, main = main)
  polygon(c(xmin,xmin,4.45,4.45), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
  polygon(c(4.55,4.55,8.45,8.45), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
  polygon(c(8.55,8.55,xmax,xmax), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
  mtext(at = c(2.75,6.5,10.25), c("Set 1", "Set 2", "Set 3"), side = 1, col="white", padj = -2, cex=2)
  
  polygon(c(xmin:xmax,xmax:xmin),c(apply(df, 2, quantile, probs = 0.25), rev(apply(df, 2, quantile, probs = 0.75))), col = lightcolor, border = FALSE)
  lines(apply(df, 2, mean), col=darkcolor, lty = 1)
  lines(apply(df, 2, quantile, probs = 0.25), col=darkcolor, lty = 2)
  lines(apply(df, 2, quantile, probs = 0.75), col=darkcolor, lty = 2)
  apply(df, 1, function(row) {lines(row)})
}

# Open Hexagon scores
gs_high = data.frame(matrix(unlist(map(list.map(results_high, test_results$distraction$open_hexagon), function(study) { list.remove(study, c("-1", "-2", 100, 101, 102)) })), nrow = 10, byrow = TRUE)) # without dry and test runs, just 0-11
a = map(list.map(results_high, test_results$distraction$open_hexagon), function(study) { list.remove(study, c("-1", "-2")) })
unlist(a[[1]])
gs_high_dry = data.frame(matrix(unlist(map(list.map(results_high, test_results$distraction$open_hexagon), function(study) { list.remove(study, c("-1", "-2", 0:11)) })), nrow = 10, byrow = TRUE)) # only dry run
gs_high_all = data.frame(matrix(unlist(map(list.map(results_high, test_results$distraction$open_hexagon), function(study) { list.remove(study, c("-1", "-2")) })), nrow = 10, byrow = TRUE)) # all

# Game runs
phl_df_plot(gs_high, 1, 12, -300, 0, "lightpink", "red2", "Training session", "Open Hexagon score (number of deaths as negative score)", 6.5, -250, "Group B")

# all runs
pdf(file = "plots/open_hexagon_scores.pdf", width = 5)
df = gs_high_all
darkcolor = "red2"
lightcolor = "lightpink"
xmin = 1
xmax = 15
ymin = -300
ymax = 0
plot(range(xmin, xmax), range(ymin, ymax), type = "n", xlab = "Training session", ylab = "Open Hexagon score (number of deaths as negative score)", main = "Group B", xaxt = "n")
polygon(c(1,1,4.5,4.5), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
polygon(c(5.5,5.5,9.5,9.5), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
polygon(c(10.5,10.5,14.5,14.5), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
mtext(c("Set 1", "Dry 1", "Set 2", "Dry 2", "Set 3", "Dry 3"), side = 1, at = c(2.5,5,7.5, 10, 12.5, 15), padj = 1)
polygon(c(xmin:xmax,xmax:xmin),c(apply(df, 2, quantile, probs = 0.25), rev(apply(df, 2, quantile, probs = 0.75))), col = lightcolor, border = FALSE)
lines(apply(df, 2, median), col=darkcolor, lty=5)
lines(apply(df, 2, mean), col=darkcolor, lty = 1)
lines(apply(df, 2, quantile, probs = 0.25), col=darkcolor, lty = 2)
lines(apply(df, 2, quantile, probs = 0.75), col=darkcolor, lty = 2)
lines(apply(df, 2, min), col=darkcolor, lty=3)
lines(apply(df, 2, max), col=darkcolor, lty=3)
legend(8, -250, legend=c("Mean", "Median", "Upper/lower quartile", "Min/max"), col=c(darkcolor, darkcolor, darkcolor, darkcolor), lty=c(1,5,2,3), cex=0.8, bg = "white")
dev.off()

pdf(file = "plots/open_hexagon_scores_full.pdf")
df = gs_high_all
darkcolor = "red2"
lightcolor = "lightpink"
xmin = 1
xmax = 15
ymin = -300
ymax = 0
plot(range(xmin, xmax), range(ymin, ymax), type = "n", xlab = "Training session", ylab = "Open Hexagon score (number of deaths as negative score)", main = "Group B", xaxt = "n")
polygon(c(1,1,4.5,4.5), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
polygon(c(5.5,5.5,9.5,9.5), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
polygon(c(10.5,10.5,14.5,14.5), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
mtext(c("Set 1", "Dry 1", "Set 2", "Dry 2", "Set 3", "Dry 3"), side = 1, at = c(2.5,5,7.5, 10, 12.5, 15), padj = 1)
polygon(c(xmin:xmax,xmax:xmin),c(apply(df, 2, quantile, probs = 0.25), rev(apply(df, 2, quantile, probs = 0.75))), col = lightcolor, border = FALSE)
lines(apply(df, 2, mean), col=darkcolor, lty = 1)
lines(apply(df, 2, quantile, probs = 0.25), col=darkcolor, lty = 2)
lines(apply(df, 2, quantile, probs = 0.75), col=darkcolor, lty = 2)
#legend(8, -250, legend=c("Mean", "Median", "Upper/lower quartile", "Min/max"), col=c(darkcolor, darkcolor, darkcolor, darkcolor), lty=c(1,5,2,3), cex=0.8, bg = "white")
apply(df, 1, function(row) {lines(row)})
dev.off()


t.test(gs_high_dry$X1, gs_high$X4, alternative = "two.sided", paired = T)
t.test(gs_high_dry$X1, gs_high$X5, alternative = "two.sided", paired = T)

(mean(gs_high_dry$X1) - mean(gs_high$X4)) / mean(gs_high_dry$X1)
(mean(gs_high_dry$X1) - mean(gs_high$X5)) / mean(gs_high_dry$X1)

t.test(gs_high_dry$X2, gs_high$X8, alternative = "two.sided", paired = T)
t.test(gs_high_dry$X2, gs_high$X9, alternative = "two.sided", paired = T)

boxplot(gs_high_dry$X2, gs_high$X9)

t.test(gs_high_dry$X3, gs_high$X12, alternative = "two.sided", paired = T)

apply(gs_high, 1, function(row) {lines(row)})

mean(unlist((gs_high$X12 - gs_high$X1) / gs_high$X12))
mean(unlist((gs_high$X12 - gs_high$X1) / abs(gs_high$X12)))

# Gweled scores
gs_low = data.frame(matrix(unlist(map(list.map(results_low, test_results$distraction$gweled), function(study) { list.remove(study, c("-1", "-2", 100, 101, 102)) })), nrow = 10, byrow = TRUE)) # without dry and test runs, just 0-11
gs_low_all = data.frame(matrix(unlist(map(list.map(results_low, test_results$distraction$gweled), function(study) { list.remove(study, c("-1", "-2")) })), nrow = 10, byrow = TRUE)) # without starter scores

phl_df_plot(gs_low, 1, 12, 0, 550, "lightyellow", "yellow4", "Training session", "Gweled score (number of symbols removed)", 6.5, 100, "Group A")

pdf(file = "plots/gweled_scores.pdf", width = 5)
df = gs_low_all
darkcolor = "yellow4"
lightcolor = "lightyellow"
xmin = 1
xmax = 15
ymin = 0
ymax = 560
plot(range(xmin, xmax), range(ymin, ymax), type = "n", xlab = "Training session", ylab = "Gweled score (number of symbols removed)", main = "Group A", xaxt = "n")
polygon(c(1,1,4.5,4.5), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
polygon(c(5.5,5.5,9.5,9.5), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
polygon(c(10.5,10.5,14.5,14.5), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
mtext(c("Set 1", "Dry 1", "Set 2", "Dry 2", "Set 3", "Dry 3"), side = 1, at = c(2.5,5,7.5, 10, 12.5, 15), padj = 1)
polygon(c(xmin:xmax,xmax:xmin),c(apply(df, 2, quantile, probs = 0.25), rev(apply(df, 2, quantile, probs = 0.75))), col = lightcolor, border = FALSE)
lines(apply(df, 2, median), col=darkcolor, lty=5)
lines(apply(df, 2, mean), col=darkcolor, lty = 1)
lines(apply(df, 2, quantile, probs = 0.25), col=darkcolor, lty = 2)
lines(apply(df, 2, quantile, probs = 0.75), col=darkcolor, lty = 2)
lines(apply(df, 2, min), col=darkcolor, lty=3)
lines(apply(df, 2, max), col=darkcolor, lty=3)
legend(8, 100, legend=c("Mean", "Median", "Upper/lower quartile", "Min/max"), col=c(darkcolor, darkcolor, darkcolor, darkcolor), lty=c(1,5,2,3), cex=0.8, bg = "white")
dev.off()

pdf(file = "plots/gweled_scores_full.pdf")
df = gs_low_all
darkcolor = "yellow4"
lightcolor = "lightyellow"
xmin = 1
xmax = 15
ymin = 0
ymax = 560
plot(range(xmin, xmax), range(ymin, ymax), type = "n", xlab = "Training session", ylab = "Gweled score (number of symbols removed)", main = "Group A", xaxt = "n")
polygon(c(1,1,4.5,4.5), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
polygon(c(5.5,5.5,9.5,9.5), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
polygon(c(10.5,10.5,14.5,14.5), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
mtext(c("Set 1", "Dry 1", "Set 2", "Dry 2", "Set 3", "Dry 3"), side = 1, at = c(2.5,5,7.5, 10, 12.5, 15), padj = 1)
polygon(c(xmin:xmax,xmax:xmin),c(apply(df, 2, quantile, probs = 0.25), rev(apply(df, 2, quantile, probs = 0.75))), col = lightcolor, border = FALSE)
lines(apply(df, 2, mean), col=darkcolor, lty = 1)
lines(apply(df, 2, quantile, probs = 0.25), col=darkcolor, lty = 2)
lines(apply(df, 2, quantile, probs = 0.75), col=darkcolor, lty = 2)
#legend(8, 100, legend=c("Mean", "Median", "Upper/lower quartile", "Min/max"), col=c(darkcolor, darkcolor, darkcolor, darkcolor), lty=c(1,5,2,3), cex=0.8, bg = "white")
apply(df, 1, function(row) {lines(row)})
dev.off()


####### Calculations for document #######
# mean(gs_low$X1)
# mean(gs_low$X12)
# sd(gs_low$X1)
# sd(gs_low$X12)
# 
# t.test(gs_low$X1, gs_low$X12, paired = TRUE)
# 
# mean(unlist((gs_high$X12 - gs_high$X1) / gs_high$X1))
# mean(gs_high$X1)
# mean(gs_high$X12)
# sd(gs_high$X1)
# sd(gs_high$X12)
# 
# t.test(gs_high$X1, gs_high$X12, paired = TRUE)
# 
# average_recognize_score_l = map(results_low, function(list){ mean(unlist(map(list$test_results$recognize, function(subset) { mean(unlist(map(subset, 'correct'))) }))) })
# average_recognize_score_h = map(results_high, function(list){ mean(unlist(map(list$test_results$recognize, function(subset) { mean(unlist(map(subset, 'correct'))) }))) })
# average_recognize_score_n = map(results_no, function(list){ mean(unlist(map(list$test_results$recognize, function(subset) { mean(unlist(map(subset, 'correct'))) }))) })
# average_recognize_score = map(results, function(list){ mean(unlist(map(list$test_results$recognize, function(subset) { mean(unlist(map(subset, 'correct'))) }))) })
# 
# average_construct_score_l = map(results_low, function(list){ mean(unlist(map(list$test_results$construct, function(subset) { mean(unlist(map(subset, 'correct'))) }))) })
# average_construct_score_h = map(results_high, function(list){ mean(unlist(map(list$test_results$construct, function(subset) { mean(unlist(map(subset, 'correct'))) }))) })
# average_construct_score_n = map(results_no, function(list){ mean(unlist(map(list$test_results$construct, function(subset) { mean(unlist(map(subset, 'correct'))) }))) })
# average_construct_score = map(results, function(list){ mean(unlist(map(list$test_results$construct, function(subset) { mean(unlist(map(subset, 'correct'))) }))) })
# 
# cor(unlist(average_recognize_score_l), unlist(rowMeans(gs_low)), method = "spearman")
# cor(unlist(average_recognize_score_h), unlist(rowMeans(gs_high)), method = "spearman")
# 
# plot(x = unlist(average_recognize_score_l), y = unlist(rowMeans(gs_low)))
# plot(x = unlist(average_recognize_score_h), y = unlist(rowMeans(gs_high)))
# 
# cor(unlist(average_construct_score_l), unlist(rowMeans(gs_low)), method = "spearman")
# cor(unlist(average_construct_score_h), unlist(rowMeans(gs_high)), method = "spearman")
# 
# plot(x = unlist(average_construct_score_l), y = unlist(rowMeans(gs_low)))
# plot(x = unlist(average_construct_score_h), y = unlist(rowMeans(gs_high)))
###