#!/usr/bin/env Rscript
setwd('/opt/BA/Evaluation')
source('load_results.R')
library(vioplot)

# Questions for all groups
survey_too_strong_l <- map(results_low, function(list) {list$survey_post_results$`Die Vibration war zu stark.`})
survey_too_weak_l   <- map(results_low, function(list) {list$survey_post_results$`Die Vibration war zu schwach.`})
survey_annoying_l   <- map(results_low, function(list) {list$survey_post_results$`Die Vibration war störend/unangenehm.`})
survey_discern_l    <- map(results_low, function(list) {list$survey_post_results$`Lange und kurze Vibrationen waren gut zu unterscheiden.`})
survey_audible_l    <- map(results_low, function(list) {list$survey_post_results$`Ich konnte das Summen der Vibrationsmotoren hören.`})
survey_audib_help_l <- map(results_low, function(list) {list$survey_post_results$`Das Summen der Motoren zu Hören hat mir bei der Wahrnehmung der Muster geholfen.`})
survey_motivation_l <- map(results_low, function(list) {list$survey_post_results$`Ich war motiviert, die Muster bestmöglichst zu lernen.`})
survey_recognize_l  <- map(results_low, function(list) {list$survey_post_results$`Ich konnte im Test die Muster gut erkennen.`})
survey_recall_l     <- map(results_low, function(list) {list$survey_post_results$`Ich konnte im Test die Muster gut wiedergeben.`})

survey_too_strong_h <- map(results_high, function(list) {list$survey_post_results$`Die Vibration war zu stark.`})
survey_too_weak_h   <- map(results_high, function(list) {list$survey_post_results$`Die Vibration war zu schwach.`})
survey_annoying_h   <- map(results_high, function(list) {list$survey_post_results$`Die Vibration war störend/unangenehm.`})
survey_discern_h    <- map(results_high, function(list) {list$survey_post_results$`Lange und kurze Vibrationen waren gut zu unterscheiden.`})
survey_audible_h    <- map(results_high, function(list) {list$survey_post_results$`Ich konnte das Summen der Vibrationsmotoren hören.`})
survey_audib_help_h <- map(results_high, function(list) {list$survey_post_results$`Das Summen der Motoren zu Hören hat mir bei der Wahrnehmung der Muster geholfen.`})
survey_motivation_h <- map(results_high, function(list) {list$survey_post_results$`Ich war motiviert, die Muster bestmöglichst zu lernen.`})
survey_recognize_h  <- map(results_high, function(list) {list$survey_post_results$`Ich konnte im Test die Muster gut erkennen.`})
survey_recall_h     <- map(results_high, function(list) {list$survey_post_results$`Ich konnte im Test die Muster gut wiedergeben.`})

survey_too_strong_n <- map(results_no, function(list) {list$survey_post_results$`Die Vibration war zu stark.`})
survey_too_weak_n   <- map(results_no, function(list) {list$survey_post_results$`Die Vibration war zu schwach.`})
survey_annoying_n   <- map(results_no, function(list) {list$survey_post_results$`Die Vibration war störend/unangenehm.`})
survey_discern_n    <- map(results_no, function(list) {list$survey_post_results$`Lange und kurze Vibrationen waren gut zu unterscheiden.`})
survey_audible_n    <- map(results_no, function(list) {list$survey_post_results$`Ich konnte das Summen der Vibrationsmotoren hören.`})
survey_audib_help_n <- map(results_no, function(list) {list$survey_post_results$`Das Summen der Motoren zu Hören hat mir bei der Wahrnehmung der Muster geholfen.`})
survey_motivation_n <- map(results_no, function(list) {list$survey_post_results$`Ich war motiviert, die Muster bestmöglichst zu lernen.`})
survey_recognize_n  <- map(results_no, function(list) {list$survey_post_results$`Ich konnte im Test die Muster gut erkennen.`})
survey_recall_n     <- map(results_no, function(list) {list$survey_post_results$`Ich konnte im Test die Muster gut wiedergeben.`})

pdf(file = "plots/survey_common_low.pdf", width = 6.5)
par(las=1, oma=c(2,18,0,0))
plot(0:1, 0:1, xlim=c(1,5), ylim=range(0,10), type="n", axes=FALSE, ann=FALSE)
vioplot(unlist(survey_recall_l), unlist(survey_recognize_l), unlist(survey_motivation_l), unlist(survey_audib_help_l), unlist(survey_audible_l),
        unlist(survey_discern_l), unlist(survey_annoying_l),unlist(survey_too_weak_l), unlist(survey_too_strong_l), horizontal = TRUE, col='lightyellow', add = TRUE)
axis(side=1,at=1:5,labels=c("agree", "weak agree", "neutral", "weak disagree", "disagree"), las = 2)
axis(side=2,at=1:9,rev(c("The vibration was too strong", "The vibration was too weak", "The vibration was uncomfortable", "Long and short vibrations were easy to tell apart", "I could hear the vibration motors", "Hearing the motors was helpful at sensing the patterns", "I was motivated to learn the patterns as best as possible", "In the test, I could recognize the patterns well", "In the test, I could recall the patterns well")))
text(3,11,"Group A", xpd = TRUE, cex = 1.1, font = 2, adj = 0.5)
dev.off()

pdf(file = "plots/survey_common_high.pdf", width = 2.9)
par(las=1, oma=c(2,0,0,0))
plot(0:1, 0:1, xlim=c(1,5), ylim=range(0,10), type="n", axes=FALSE, ann=FALSE)
vioplot(unlist(survey_recall_h), unlist(survey_recognize_h), unlist(survey_motivation_h), unlist(survey_audib_help_h), unlist(survey_audible_h),
        unlist(survey_discern_h), unlist(survey_annoying_h),unlist(survey_too_weak_h), unlist(survey_too_strong_h), horizontal = TRUE, col='lightpink', add = TRUE)
axis(side=1,at=1:5,labels=c("agree", "weak agree", "neutral", "weak disagree", "disagree"), las=2)
text(3,11,"Group B", xpd = TRUE, cex = 1.1, font=2, adj = 0.5)
dev.off()

pdf(file = "plots/survey_common_no.pdf", width = 2.9)
par(las=1, oma=c(2,0,0,0))
plot(0:1, 0:1, xlim=c(1,5), ylim=range(0,10), type="n", axes=FALSE, ann=FALSE)
vioplot(unlist(survey_recall_n), unlist(survey_recognize_n), unlist(survey_motivation_n), unlist(survey_audib_help_n), unlist(survey_audible_n),
        unlist(survey_discern_n), unlist(survey_annoying_n),unlist(survey_too_weak_n), unlist(survey_too_strong_n), horizontal = TRUE, col='lightgreen', add = TRUE)
axis(side=1,at=1:5,labels=c("agree", "weak agree", "neutral", "weak disagree", "disagree"), las=2)
text(3,11,"Group C", xpd = TRUE, cex = 1.1, font=2, adj = 0.5)
dev.off()


# Questions of only Low- & HighDistraction
survey_knew_game_l  <- map(results_low, function(list) {list$survey_post_results$`Ich kannte das Spiel bereits gut`})
survey_focus_vib_l  <- map(results_low, function(list) {list$survey_post_results$`Ich habe mich während des Spiels auf die Vibrationsmuster konzentriert.`})
survey_focus_game_l <- map(results_low, function(list) {list$survey_post_results$`Ich habe mich auf das Spiel konzentriert.`})
survey_distr_vib_l  <- map(results_low, function(list) {list$survey_post_results$`Die Vibration hat vom Spiel abgelenkt.`})
survey_distr_num_l  <- map(results_low, function(list) {list$survey_post_results$`Die Zahlwörter haben vom Spiel abgelenkt.`})
survey_req_focus_l  <- map(results_low, function(list) {list$survey_post_results$`Das Spiel hat meine volle Aufmerksamkeit erfordert.`})
survey_fun_l        <- map(results_low, function(list) {list$survey_post_results$`Das Spiel hat mir Spaß gemacht.`})
survey_motivation_game_l <- map(results_low, function(list) {list$survey_post_results$`Ich war motiviert, das Spiel bestmöglichst zu spielen.`})

survey_knew_game_h  <- map(results_high, function(list) {list$survey_post_results$`Ich kannte das Spiel bereits gut`})
survey_focus_vib_h  <- map(results_high, function(list) {list$survey_post_results$`Ich habe mich während des Spiels auf die Vibrationsmuster konzentriert.`})
survey_focus_game_h <- map(results_high, function(list) {list$survey_post_results$`Ich habe mich auf das Spiel konzentriert.`})
survey_distr_vib_h  <- map(results_high, function(list) {list$survey_post_results$`Die Vibration hat vom Spiel abgelenkt.`})
survey_distr_num_h  <- map(results_high, function(list) {list$survey_post_results$`Die Zahlwörter haben vom Spiel abgelenkt.`})
survey_req_focus_h  <- map(results_high, function(list) {list$survey_post_results$`Das Spiel hat meine volle Aufmerksamkeit erfordert.`})
survey_fun_h        <- map(results_high, function(list) {list$survey_post_results$`Das Spiel hat mir Spaß gemacht.`})
survey_motivation_game_h <- map(results_high, function(list) {list$survey_post_results$`Ich war motiviert, das Spiel bestmöglichst zu spielen.`})

pdf(file = "plots/survey_distraction_low.pdf", width = 6.3)
par(las=1, oma=c(2,13,0,0))
plot(0:1, 0:1, xlim=c(1,5), ylim=range(0,9), type="n", axes=FALSE, ann=FALSE)
vioplot(unlist(survey_knew_game_l), unlist(survey_focus_vib_l), unlist(survey_focus_game_l), unlist(survey_distr_vib_l), unlist(survey_distr_num_l),
        unlist(survey_req_focus_l), unlist(survey_fun_l), unlist(survey_motivation_game_l), horizontal = TRUE, col='khaki1', add = TRUE)
axis(side=1,at=1:5,labels=c("agree", "weak agree", "neutral", "weak disagree", "disagree"), las = 2)
axis(side=2,at=1:8,c("I knew the game", "I focused on the vibration", "I focused on the game", "Vibration was distracting", "Hearing numbers was distracting", "The game required full attention", "The game was fun", "I was motivated to play the best"))
text(3,10,"Group A", xpd = TRUE, cex = 1.1, font = 2, adj = 0.5)
dev.off()

pdf(file = "plots/survey_distraction_high.pdf", width = 3.7)
par(las=1, oma=c(2,0,0,0))
plot(0:1, 0:1, xlim=c(1,5), ylim=range(0,9), type="n", axes=FALSE, ann=FALSE)
vioplot(unlist(survey_knew_game_h), unlist(survey_focus_vib_h), unlist(survey_focus_game_h), unlist(survey_distr_vib_h), unlist(survey_distr_num_h),
        unlist(survey_req_focus_h), unlist(survey_fun_h), unlist(survey_motivation_game_h), horizontal = TRUE, col='lightpink', add = TRUE)
axis(side=1,at=1:5,labels=c("agree", "weak agree", "neutral", "weak disagree", "disagree"), las=2)
text(3,10,"Group B", xpd = TRUE, cex = 1.1, font=2, adj = 0.5)
dev.off()

# only for NoDistraction
survey_too_fast   <- map(results, function(list) {list$survey_post_results$`Die Muster folgten zu schnell aufeinander.`})
survey_too_slow   <- map(results, function(list) {list$survey_post_results$`Die Muster folgten zu langsam aufeinander.`})
survey_remember   <- map(results, function(list) {list$survey_post_results$`Ich konnte mir die Muster leicht merken.`})
survey_mnemonic   <- map(results, function(list) {list$survey_post_results$`Ich habe Eselsbrücken verwendet.`})

pdf(file = "plots/survey_no_distraction.pdf", width = 7, height = 5)
par(las=1, oma=c(2,12,0,0))
plot(0:1, 0:1, xlim=c(1,5), ylim=range(0,5), type="n", axes=FALSE, ann=FALSE)
vioplot(unlist(survey_too_fast), unlist(survey_too_slow), unlist(survey_remember), unlist(survey_mnemonic), horizontal = TRUE, col='lightgreen', add = TRUE)
axis(side=1,at=1:5,labels=c("agree", "weak agree", "neutral", "weak disagree", "disagree"), las = 2)
axis(side=2,at=1:4,c("The pattern sequence was too fast", "The pattern sequence was too slow", "I could remember the patterns easily", "I used mnemonic devices"))
text(3,10,"Group C", xpd = TRUE, cex = 1.1, font = 2, adj = 0.5)
dev.off()


####### common questions as boxplot
#par(oma=c(0,15,0,0))
#boxplot(names = c("The vibration was too strong", "The vibration was too weak", "The vibration was uncomfortable", "Long and short vibrations were easy to tell apart", "I could hear the vibration motors", "Hearing the vibration motors was helpful at sensing the patterns", "I was motivated to learn the patterns as best as possible", "In the test, I could recognize the patterns well", "In the test, I could recall the patterns well"), las=1,
#        unlist(survey_too_strong), unlist(survey_too_weak), unlist(survey_annoying), unlist(survey_discern), unlist(survey_audible),
#        unlist(survey_audib_help), unlist(survey_motivation), unlist(survey_recognize), unlist(survey_recall), horizontal = TRUE)
###

####### Calculations for document #######
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
# game_score_l = map(map(list.map(results_low, test_results$distraction$gweled), function(study) { list.remove(study, c("-1", "-2")) }), function(list) {mean(unlist(list))})
# game_score_h = map(map(list.map(results_high, test_results$distraction$open_hexagon), function(study) { list.remove(study, c("-1", "-2")) }), function(list) {mean(unlist(list))})
# 
# survey_recognize  <- map(results, function(list) {list$survey_post_results$`Ich konnte im Test die Muster gut erkennen.`})
# survey_recall     <- map(results, function(list) {list$survey_post_results$`Ich konnte im Test die Muster gut wiedergeben.`})
# 
# cor(unlist(game_score_l), unlist(survey_knew_game_l), method = 'spearman')
# plot(x = unlist(game_score_l), y = unlist(survey_knew_game_l))
# 
# t.test(unlist(game_score_l[4:10]), unlist(game_score_l[1:3]))
# 
# cor(unlist(game_score_h), unlist(survey_knew_game_h), method = 'spearman')
# plot(x = unlist(game_score_h), y = unlist(survey_knew_game_h))
# 
# cor(unlist(game_score_l), unlist(survey_focus_vib_l), method = 'spearman')
# cor(unlist(game_score_h), unlist(survey_focus_vib_h), method = 'spearman')
# 
# cor(unlist(game_score_l), unlist(survey_focus_game_l), method = 'spearman')
# cor(unlist(game_score_h), unlist(survey_focus_game_h), method = 'spearman')
# 
# cor(unlist(game_score_l), unlist(survey_distr_vib_l), method = 'spearman')
# cor(unlist(game_score_h), unlist(survey_distr_vib_h), method = 'spearman')
# 
# cor(unlist(game_score_l), unlist(survey_distr_num_l), method = 'spearman')
# cor(unlist(game_score_h), unlist(survey_distr_num_h), method = 'spearman')
# 
# cor(unlist(game_score_l), unlist(survey_req_focus_l), method = 'spearman')
# plot(x = unlist(game_score_l), y = unlist(survey_req_focus_l))
# cor(unlist(game_score_h), unlist(survey_req_focus_h), method = 'spearman')
# plot(x = unlist(game_score_h), y = unlist(survey_req_focus_h))
# 
# cor(unlist(game_score_l), unlist(survey_fun_l), method = 'spearman')
# cor(unlist(game_score_h), unlist(survey_fun_h), method = 'spearman')
# 
# cor(unlist(game_score_l), unlist(survey_motivation_game_l), method = 'spearman')
# cor(unlist(game_score_h), unlist(survey_motivation_game_h), method = 'spearman')
# 
# cor(unlist(survey_recognize), unlist(average_recognize_score), method='spearman')
# plot(x = unlist(survey_recognize), y=unlist(average_recognize_score))
# cor(unlist(survey_recall), unlist(average_construct_score), method='spearman')
# plot(x = unlist(survey_recall), y=unlist(average_construct_score))
###