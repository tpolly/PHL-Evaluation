#!/usr/bin/env Rscript
setwd("/opt/BA/Evaluation")
source('load_results.R')

mrd <- function(results) {
  subsetscore <- function(subset) {
    if(is.null(subset$pattern_distance)) { # user entered null
      if(is.null(subset$digit)) { # digit is also null
        return(0) # correct
      } else {
        return(1) # false
      }
    } else {
      return(min(subset$pattern_distance / length(subset$displayed_pattern), 1)) # cap error score at 1
    }
  }
  
  setscore <- function(set) {
    set_without_null_patterns <- list.filter(set, !is.null(digit))
    return(mean(unlist(map(set_without_null_patterns, subsetscore))))
  }
  
  mean_recognize_distance <- list.map(results, map(test_results$recognize, setscore))
  
  df <- data.frame(matrix(
    unlist(mean_recognize_distance),
    nrow = 11,
    byrow = TRUE
  ))
  
  return(df)
}

mcd <- function(results) {
  subsetscore <- function(subset) {
    return(min(subset$distance / length(subset$correct_answer), 1)) # cap error score at 1
  }
  
  setscore <- function(set) {
    return(mean(unlist(map(set, subsetscore))))
  }
  
  mean_construct_distance <- list.map(results, map(test_results$construct, setscore))
  
  df <- data.frame(matrix(
    unlist(mean_construct_distance),
    nrow = 11,
    byrow = TRUE
  ))
  
  return(df)
}

mcd_all <- mcd(results_with_retest)
mcdiff <-(mcd_all$X12 - mcd_all$X13)
mean(mcdiff)

mrd_all <- mrd(results_with_retest)
mrdiff <-(mrd_all$X12 - mrd_all$X13)
mean(mrdiff)

pdf(file = "plots/retest_recall.pdf", width = 5)
barplot(c(mcdiff[3], mcdiff[6], mcdiff[8], mcdiff[11], mcdiff[1], mcdiff[2], mcdiff[5], mcdiff[7], mcdiff[10], mcdiff[4], mcdiff[9]), col = rev(sort(unlist(list.map(results_with_retest, color)))), xlab="Participant", ylab="Score difference", main="Recall test")
legend(9, -0.3, legend=c("Group A", "Group B", "Group C"), fill=c("lightyellow", "lightpink", "lightgreen"), lty=c(0,0,0), bg="white")
dev.off()

pdf(file = "plots/retest_recognize.pdf", width = 5)
barplot(c(mrdiff[3], mrdiff[6], mrdiff[8], mrdiff[11], mrdiff[1], mrdiff[2], mrdiff[5], mrdiff[7], mrdiff[10], mrdiff[4], mrdiff[9]), col = rev(sort(unlist(list.map(results_with_retest, color)))), xlab="Participant", ylab="Score difference", main="Recognize test")
legend(9, -0.2, legend=c("Group A", "Group B", "Group C"), fill=c("lightyellow", "lightpink", "lightgreen"), lty=c(0,0,0), bg="white")
dev.off()
