#!/usr/bin/env Rscript
source('load_results.R')

comments <- map(results, function(list) {list$survey_post_results$comment})
