#!/usr/bin/env Rscript
setwd("/opt/BA/Evaluation")
source('load_results.R')

phl_df_plot <- function(df, xmin, xmax, ymin, ymax, lightcolor, darkcolor, xlab, ylab, xleg, yleg, main) {
  plot(range(xmin, xmax), range(ymin, ymax), type = "n", xlab = xlab, ylab = ylab, main = main)
  polygon(c(xmin,xmin,4.45,4.45), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
  polygon(c(4.55,4.55,8.45,8.45), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
  polygon(c(8.55,8.55,xmax,xmax), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
  mtext(at = c(2.75,6.5,10.25), c("Set 1", "Set 2", "Set 3"), side = 1, col="white", padj = -2, cex=2)
  #apply(df, 1, function(row) {lines(row, col="gray90")})
  polygon(c(xmin:xmax,xmax:xmin),c(apply(df, 2, quantile, probs = 0.25), rev(apply(df, 2, quantile, probs = 0.75))), col = lightcolor, border = FALSE)
  lines(apply(df, 2, median), col=darkcolor, lty=5)
  lines(apply(df, 2, mean), col=darkcolor, lty = 1)
  lines(apply(df, 2, quantile, probs = 0.25), col=darkcolor, lty = 2)
  lines(apply(df, 2, quantile, probs = 0.75), col=darkcolor, lty = 2)
  lines(apply(df, 2, min), col=darkcolor, lty=3)
  lines(apply(df, 2, max), col=darkcolor, lty=3)
  legend(xleg, yleg, legend=c("Mean", "Median", "Upper/lower quartile", "Min/max"), col=c(darkcolor, darkcolor, darkcolor, darkcolor), lty=c(1,5,2,3), cex=0.8, bg="white")
}

phl_df_full_plot <- function(df, xmin, xmax, ymin, ymax, lightcolor, darkcolor, xlab, ylab, xleg, yleg, main) {
  plot(range(xmin, xmax), range(ymin, ymax), type = "n", xlab = xlab, ylab = ylab, main = main)
  polygon(c(xmin,xmin,4.45,4.45), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
  polygon(c(4.55,4.55,8.45,8.45), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
  polygon(c(8.55,8.55,xmax,xmax), c(ymin, ymax, ymax, ymin), col="gray95", border = FALSE)
  mtext(at = c(2.75,6.5,10.25), c("Set 1", "Set 2", "Set 3"), side = 1, col="white", padj = -2, cex=2)
  
  polygon(c(xmin:xmax,xmax:xmin),c(apply(df, 2, quantile, probs = 0.25), rev(apply(df, 2, quantile, probs = 0.75))), col = lightcolor, border = FALSE)
  lines(apply(df, 2, median), col=darkcolor, lty = 5)
  lines(apply(df, 2, mean), col=darkcolor, lty = 1)
  lines(apply(df, 2, quantile, probs = 0.25), col=darkcolor, lty = 2)
  lines(apply(df, 2, quantile, probs = 0.75), col=darkcolor, lty = 2)
  apply(df, 1, function(row) {lines(row)})
}

mrd_with_null_patterns <- function(results) {
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
    return(mean(unlist(map(set, subsetscore))))
  }
  
  mean_recognize_distance <- list.map(results, map(test_results$recognize, setscore))
  
  mean_recognize_distance_no_retest <-
    map(mean_recognize_distance, function(study) {
      list.remove(study, 13)
    })
  
  df <- data.frame(matrix(
    unlist(mean_recognize_distance_no_retest),
    nrow = length(results),
    byrow = TRUE
  ))
  
  return(df)
}

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
  
  mean_recognize_distance_no_retest <-
    map(mean_recognize_distance, function(study) {
      list.remove(study, 13)
    })
  
  df <- data.frame(matrix(
    unlist(mean_recognize_distance_no_retest),
    nrow = length(results),
    byrow = TRUE
  ))
  
  return(df)
}

mrs_with_null_patterns <- function(results) {
  return(data.frame(matrix(unlist(map(list.map(results, map(test_results$recognize, function(set) { mean(unlist(list.map(set, correct))) })), function(study) { list.remove(study, 13) })), nrow = length(results), byrow = TRUE)))
}

mrs <- function(results) {
  return(data.frame(matrix(unlist(map(map(list.map(results, map(test_results$recognize, function(set) { list.filter(set, !is.null(digit)) })), function(study) { map(study, function(set) {mean(unlist(list.map(set, correct)) ) }) }), function(study) {list.remove(study, 13)})), nrow = length(results), byrow = TRUE)))
}

mcd <- function(results) {
  subsetscore <- function(subset) {
    return(min(subset$distance / length(subset$correct_answer), 1)) # cap error score at 1
  }
  
  setscore <- function(set) {
    return(mean(unlist(map(set, subsetscore))))
  }
  
  mean_construct_distance <- list.map(results, map(test_results$construct, setscore))
  
  mean_construct_distance_no_retest <-
    map(mean_construct_distance, function(study) {
      list.remove(study, 13)
    })
  
  df <- data.frame(matrix(
    unlist(mean_construct_distance_no_retest),
    nrow = length(results),
    byrow = TRUE
  ))
  
  return(df)
}

mcs <- function(results) {
  data.frame(matrix(unlist(map(list.map(results, map(test_results$construct, function(set) { mean(unlist(list.map(set, correct))) })), function(study) {list.remove(study, 13) })), nrow = length(results), byrow = TRUE))
}

mrplc <- function(results) { # mean recognize pattern length correct
  digitscore <- function(subset) {
    if(is.null(subset$pattern_distance)) { # user entered null
      if(is.null(subset$digit)) { # digit is also null
        return(1) # correct
      } else {
        return(0) # false
      }
    } else {
      return(ifelse(length(subset$displayed_pattern) == length(subset$user_answer), 1, 0))
    }
  }
  
  setscore <- function(set) {
    set_without_null_patterns <- list.filter(set, !is.null(digit))
    return(mean(unlist(map(set_without_null_patterns, digitscore))))
  }
  
  mean_recognize_distance <- list.map(results, map(test_results$recognize, setscore))
  
  mean_recognize_distance_no_retest <-
    map(mean_recognize_distance, function(study) {
      list.remove(study, 13)
    })
  
  df <- data.frame(matrix(
    unlist(mean_recognize_distance_no_retest),
    nrow = length(results),
    byrow = TRUE
  ))
  
  return(df)
}

mccpl <- function(results) { # mean recall correct pattern's lengths
  a <- map(results, function(study) {map(study$test_results$construct, function(set) {
    all <- unlist(list.map(list.filter(set, correct), length(correct_answer)))
    if (is_empty(all)) {
      return (0)
    } else {
      return (mean(all))
    }
  } )})
  return(data.frame(matrix(
    unlist(map(a, function(s) {list.remove(s, 13)})),
    nrow = length(results),
    byrow = TRUE
  )))
}

setscore <- function(set, l) { 
  length(names(
    list.filter(
      list.filter(set, length(correct_answer) == l),
      correct == TRUE
    )
  ))
}

meanscore <- function(results) {
  map(c(1,2,3,4), function(l) {
    mean(unlist(map(results, function(study) {
      setscore(study$test_results$construct$`11`, l)
    })))
  })
}

lightcolor <- function(dist) {
  if (dist == "no") {
    return("palegreen")
  } else if (dist == "low" || dist == "low_with_feedback") {
    return("lightyellow")
  } else {
    return("lightpink")
  }
}

darkcolor <- function(dist) {
  if (dist == "no") {
    return("green3")
  } else if (dist == "low" || dist == "low_with_feedback") {
    return("yellow4")
  } else {
    return("red2")
  }
}

group <- function(dist) {
  if (dist == "no") {
    return("C")
  } else if (dist == "low") {
    return("A")
  } else if (dist == "high") {
    return("B")
  } else if (dist == "low_with_feedback") {
    return("A-F")
  } else if (dist == "high_with_feedback") {
    return ("B-F")
  } else {
    stop("bad dist")
  }
}

results_h <- function(dist) { # R is stupid. Can't get it to work with a list
  if (dist == "no") {
    return(results_no)
  } else if (dist == "low") {
    return(results_low)
  } else if (dist == "high") {
    return(results_high)
  } else if (dist == "low_with_feedback") {
    return(results_low_with_feedback)
  } else if (dist == "high_with_feedback") {
    return(results_high_with_feedback)
  } else {
    stop("bad dist")
  }
}

for (dist in c("no", "low", "high", "low_with_feedback", "high_with_feedback")) {
#for (dist in c("low_with_feedback")) {
  setwd("/opt/BA/Evaluation/plots")
  # recognize
  ## using distance metric
  pdf(file = paste0(dist, "_mean_recognize_distance_with_null_patterns.pdf"))
    phl_df_plot(mrd_with_null_patterns(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recognize test mean error rate", 1, 1, paste("Group", group(dist), "Recognize Error (including non-existing patterns)"))
  dev.off()
  
  pdf(file = paste0(dist, "_mean_recognize_distance_with_null_patterns_full.pdf"))
    phl_df_full_plot(mrd_with_null_patterns(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recognize test mean error rate", 1, 1, paste("Group", group(dist), "Recognize Error (including non-existing patterns)"))
  dev.off()
  
  pdf(file = paste0(dist, "_mean_recognize_distance_without_null_patterns.pdf"))
    phl_df_plot(mrd(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recognize test mean error rate", 1, 1, paste("Group", group(dist), "Recognize Error (excluding non-existing patterns)"))
  dev.off()
  
  pdf(file = paste0(dist, "_mean_recognize_distance_without_null_patterns_5.pdf"), width = 5)
    phl_df_plot(mrd(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recognize test mean error rate", 1, 0.3, paste("Group", group(dist), "Recognize Error"))
  dev.off()
  
  pdf(file = paste0(dist, "_mean_recognize_distance_without_null_patterns_full.pdf"))
    phl_df_full_plot(mrd(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recognize test mean error rate", 1, 1, paste("Group", group(dist), "Recognize Error (excluding non-existing patterns)"))
  dev.off()
  ## using score metric
  pdf(file = paste0(dist, "_mean_recognize_score_with_null_patterns.pdf"))
    phl_df_plot(mrs_with_null_patterns(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recognize test correct patterns", 1, 1, paste("Group", group(dist), "Recognize Error (including non-existing patterns)"))
  dev.off()
  
  pdf(file = paste0(dist, "_mean_recognize_score_with_null_patterns_full.pdf"))
    phl_df_full_plot(mrs_with_null_patterns(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recognize test correct patterns", 1, 1, paste("Group", group(dist), "Recognize Error (including non-existing patterns)"))
  dev.off()
  
  pdf(file = paste0(dist, "_mean_recognize_score_without_null_patterns.pdf"))
    phl_df_plot(mrs(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recognize test correct patterns", 1, 1, paste("Group", group(dist), "Recognize Error (excluding non-existing patterns)"))
  dev.off()
  
  pdf(file = paste0(dist, "_mean_recognize_score_without_null_patterns_5.pdf"), width = 5)
    phl_df_plot(mrs(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recognize test correct patterns", 1, 0.3, paste("Group", group(dist), "Recognize Error"))
  dev.off()
  
  pdf(file = paste0(dist, "_mean_recognize_score_without_null_patterns_full.pdf"))
    phl_df_full_plot(mrs(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recognize test correct patterns", 1, 1, paste("Group", group(dist), "Recognize Error (excluding non-existing patterns)"))
  dev.off()
  
  # recall
  ## using distance metric
  pdf(file = paste0(dist, "_mean_recall_distance.pdf"))
    phl_df_plot(mcd(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recall test mean error rate", 1, 0.3, paste("Group", group(dist), "Recall Error"))
  dev.off()
  
  pdf(file = paste0(dist, "_mean_recall_distance_5.pdf"), width = 5)
    phl_df_plot(mcd(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recall test mean error rate", 1, 0.3, paste("Group", group(dist), "Recall Error"))
  dev.off()
  
  pdf(file = paste0(dist, "_mean_recall_distance_full.pdf"))
    phl_df_full_plot(mcd(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recall test mean error rate", 1, 0.3, paste("Group", group(dist), "Recall Error"))
  dev.off()
  
  ## using score metric
  pdf(file = paste0(dist, "_mean_recall_score.pdf"))
    phl_df_plot(mcs(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recall test correct patterns", 1, 0.3, paste("Group", group(dist), "Recall Error"))
  dev.off()
  
  pdf(file = paste0(dist, "_mean_recall_score_5.pdf"), width = 5)
    phl_df_plot(mcs(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recall test correct patterns", 1, 0.3, paste("Group", group(dist), "Recall Error"))
  dev.off()
  
  pdf(file = paste0(dist, "_mean_recall_score_full.pdf"))
    phl_df_full_plot(mcs(results_h(dist)), 1, 12, 0, 1, lightcolor(dist), darkcolor(dist), "Training session", "Recall test correct patterns", 1, 0.3, paste("Group", group(dist), "Recall Error"))
  dev.off()
  
  # mean pattern length of correct pattern in recall test
  pdf(file = paste0(dist, "_mean_correct_recall_pattern_length.pdf"), width = 4.4, height = 5)
    phl_df_plot(mccpl(results_h(dist)), 1, 12, 0, 5, lightcolor(dist), darkcolor(dist), "Training session", "Mean pattern length", 5 , 5, paste("Group", group(dist)))
  dev.off()
  
  pdf(file = paste0(dist, "_mean_learned_by_length.pdf"), width = 3, height = 3)
    mp <- barplot(unlist(meanscore(results_h(dist))) / c(1,2,3,4), col = lightcolor(dist), xlab = "Pattern length", ylab = "Percentage learned", main = paste("Group", group(dist)), ylim = 0:1)
    axis(1, c(1,2,3,4), at = mp)
  dev.off()
}

####### For document #######
# t.test(mrd(results_low)$X1, mrd(results_low)$X12, paired = T)
# t.test(mrd(results_high)$X1, mrd(results_high)$X12, paired = T)
# 
# t.test(mcd(results_low)$X1, mcd(results_low)$X12, paired = T)
# t.test(mcd(results_high)$X1, mcd(results_high)$X12, paired = T)
# 
# 
# plot(density(mrd(results_high)$X12), ylim = c(0,5))
# hist(mrd(results_high)$X12, add = T, breaks = 10)
# 
# 
# means <- (mrd(results_high)$X12 + mrd(results_high)$X11) / 2
# plot(density(means), ylim = c(0,5), xlim = c(0,1))
# hist(means, add = T)
# 
# means <- (mcd(results_high)$X12 + mcd(results_high)$X11) / 2
# plot(density(means), ylim = c(0,5), xlim = c(0,1))
# hist(means, add = T)
# 
# 
# means <- (mrd(results_low)$X12 + mrd(results_low)$X11) / 2
# plot(density(means), ylim = c(0,5), xlim = c(0,1))
# hist(means, add = T, breaks = 10)
# 
# means <- (mcd(results_low)$X12 + mcd(results_low)$X11) / 2
# plot(density(means), ylim = c(0,5), xlim = c(0,1))
# hist(means, add = T, breaks = 10)
#
# mean(mccpl(results_high)$X12)
# mean(mccpl(results_low)$X12)
###
