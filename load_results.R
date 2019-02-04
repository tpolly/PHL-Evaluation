#!/usr/bin/env Rscript
library('rjson')
library('purrr')
library('rlist')

origin_path = getwd()

setwd("/opt/BA/Results/")
results = list()
results_no = list()
results_low = list()
results_high = list()
results_low_with_feedback = list()
results_high_with_feedback = list()

files = c(
  list.files("with-retest", pattern="\\.json$", full.names = TRUE), 
  list.files("no-retest", pattern="\\.json$", full.names = TRUE)
)

for (file in files) {
  list <- fromJSON(file = file)
  results[length(results) + 1] <- list(list)
  if (list$distraction == 'NoDistraction') {
    results_no[length(results_no) + 1] <- list(list)
  } else if (list$distraction == 'LowDistraction') {
    results_low[length(results_low) + 1] <- list(list)
  } else if (list$distraction == 'HighDistraction') {
    results_high[length(results_high) + 1] <- list(list)
  } else if (list$distraction == 'LowDistractionWithFeedback') {
    results_low_with_feedback[length(results_low_with_feedback) + 1] <- list(list)
  } else if (list$distraction == 'HighDistractionWithFeedback') {
    results_high_with_feedback[length(results_high_with_feedback) + 1] <- list(list)
  }
}

for (i in 1:length(results)) {
  if (results[[i]]$distraction == 'NoDistraction') {
    color = 'lightgreen'
  } else if (results[[i]]$distraction == 'LowDistraction' || results[[i]]$distraction == 'LowDistractionWithFeedback') {
    color = 'lightyellow'
  } else if (results[[i]]$distraction == 'HighDistraction' || results[[i]]$distraction == 'HighDistractionWithFeedback') {
    color = 'lightpink'
  }
  results[[i]]$color <- color
}

results_with_retest = list.filter(results, !is.null(test_results$recognize$`13`))
results_no_retest = list.filter(results, is.null(test_results$recognize$`13`))

setwd(origin_path)
rm(color, file, files, i, origin_path, list)