#####################################
# Final decision tree

final_dfs <- final_df[which(final_df$sample =='c127'), ]
final_dfs <- final_df

final_dfs <- final_dfs %>% mutate(decision = case_when((final_dfs$signal_code ==  "nuc yfp") ~ 'only YFP',
                                            (final_dfs$signal_code ==  "nuc rfp") ~ 'only RFP',
                                            (final_dfs$signal_code ==  "cfp nuc") ~ 'only CFP',
                                            (final_dfs$signal_code ==  "cfp cfp nuc") ~ '2 CFP only',
                                            (final_dfs$signal_code ==  "cfp nuc rfp" & is.na(final_dfs$cr)) ~ 'problem',
                                            (final_dfs$signal_code ==  "cfp nuc rfp" & final_dfs$cr > 0.6) ~ 'Class II',
                                            (final_dfs$signal_code ==  "cfp nuc rfp" & final_dfs$cr < 0.6) ~ 'Other 1',                                                  
                                            (final_dfs$signal_code ==  "cfp nuc yfp") ~ 'no RFP',
                                            (final_dfs$signal_code ==  "cfp nuc rfp") ~ 'Other 2',
                                            (final_dfs$signal_code ==  "cfp nuc rfp yfp" & final_dfs$ry > 0.6 & final_dfs$cy > 0.6 & final_dfs$cr > 0.6) ~ 'All 3 separated',
                                            (final_dfs$signal_code ==  "cfp nuc rfp yfp" & final_dfs$ry > 0.6 & final_dfs$cy > 0.6 & final_dfs$cr < 0.6) ~ 'All 3 clumped',
                                            (final_dfs$signal_code ==  "cfp nuc rfp yfp" & final_dfs$ry > 0.6 & final_dfs$cy < 0.6) ~ 'Class IV',
                                            (final_dfs$signal_code ==  "cfp nuc rfp yfp" & final_dfs$ry < 0.6) ~ 'Class III',
                                            (final_dfs$signal_code ==  "cfp cfp nuc rfp yfp") ~ 'Other 3',
                                            (final_dfs$signal_code ==  "cfp nuc rfp rfp yfp") ~ 'Other 4',
                                            (final_dfs$signal_code ==  "cfp nuc rfp yfp yfp") ~ 'Class I',
                                            TRUE ~ 'unknow'))


decision_tree_result <- data.frame(name=c('Class I', 'Class II', 'Class III', 'Class IV', 'Other 1', 'Other 2', 'Other 3', 
                                 'Other 4', '2 CFP only', 'only CFP','only RFP', 'only YFP', 'no CFP', 'noRFP', 'All 3 clumped', 'All 3 separated', 'unknow'),
                          number=rep(0,17))

decision_tree_result[1,2] <- nrow(final_df[which(final_dfs$decision =='Class I'), ])
decision_tree_result[2,2] <- nrow(final_df[which(final_dfs$decision =='Class II'), ]) 
decision_tree_result[3,2] <- nrow(final_df[which(final_dfs$decision =='Class III' ), ])
decision_tree_result[4,2] <- nrow(final_df[which(final_dfs$decision =='Class IV'), ])
decision_tree_result[5,2] <- nrow(final_df[which(final_dfs$decision =='Other 1'), ])
decision_tree_result[6,2] <- nrow(final_df[which(final_dfs$decision =='Other 2'), ])
decision_tree_result[7,2] <- nrow(final_df[which(final_dfs$decision =='Other 3'), ])
decision_tree_result[8,2] <- nrow(final_df[which(final_dfs$decision =='Other 4'), ])
decision_tree_result[9,2] <- nrow(final_df[which(final_dfs$decision =='2 CFP only'), ])
decision_tree_result[10,2] <- nrow(final_df[which(final_dfs$decision =='only CFP'), ])
decision_tree_result[11,2] <- nrow(final_df[which(final_dfs$decision =='only RFP' ), ])
decision_tree_result[12,2] <- nrow(final_df[which(final_dfs$decision =='only YFP' ), ])
decision_tree_result[13,2] <- nrow(final_df[which(final_dfs$decision =='no CFP' ), ])
decision_tree_result[14,2] <- nrow(final_df[which(final_dfs$decision =='no RFP'), ])
decision_tree_result[15,2] <- nrow(final_df[which(final_dfs$decision =='All 3 clumped'), ])
decision_tree_result[16,2] <- nrow(final_df[which(final_dfs$decision =='All 3 separated'), ])
decision_tree_result[17,2] <- nrow(final_df[which(final_dfs$decision =='unknow'), ])

nrow(final_dfs)





#####################################################################################################################
#####################################################################################################################
######### Essais numéro 222366980563

datat_result <- data.frame(name=c('Class I', 'Class II', 'Class III', 'Class IV', 'Other 1', 'Other 2', 'Other 3', 
                                  'Other 4', '2 CFP only', 'only CFP','only RFP', 'only YFP', 'no CFP', 'noRFP',
                                  'All 3 clumped', 'All 3 separated', 'unknow'),
                           number=rep(0,17))
ClassI <- 0
ClassII <- 0
ClassIII <- 0
ClassIV <- 0
Other1 <- 0
Other2 <- 0
Other3 <- 0
Other4 <- 0
CFP2only <- 0
onlyCFP <- 0
onlyRFP <- 0
onlyYFP <- 0
noCFP <- 0
noRFP <- 0
All3clumped <- 0
All3separated <- 0
unknow <- 0



for (j in 1:nrow(final_df)){
  
  if (final_df$signal_code[j] ==  "nuc yfp") {
    onlyYFP <- onlyYFP +1
  } else if (final_df$signal_code[j] ==  "nuc rfp") {
    onlyRFP <- onlyRFP +1
  } else if (final_df$signal_code[j] ==  "cfp nuc") {
    onlyCFP <- onlyCFP +1
  } else if ((final_df$signal_code[j] ==  "cfp nuc rfp") & (final_df$cr[j] > 0.6)){
    ClassII <- ClassII +1
  } else if ((final_df$signal_code[j] ==  "cfp nuc rfp") & (final_df$cr[j] < 0.6)){
    Other1 <- Other1 +1
  } else if (final_df$signal_code[j] ==  "cfp nuc yfp" ){
    noRFP <- noRFP +1 
  } else if (final_df$signal_code[j] ==  "cfp nuc rfp" ){
    Other2 <- Other2 +1 
  } else if ((final_df$signal_code[j] ==  "cfp nuc rfp yfp") & 
             (final_df$ry[j] > 0.6) & (final_df$cy[j] > 0.6) & (final_df$cr[j] > 0.6)){
    All3separated <- All3separated +1
  } else if ((final_df$signal_code[j] ==  "cfp nuc rfp yfp") & 
             (final_df$ry[j] > 0.6) & (final_df$cy[j] > 0.6) & (final_df$cr[j] < 0.6)){
    All3clumped <- All3clumped +1
  } else if ((final_df$signal_code[j] ==  "cfp nuc rfp yfp") & 
             (final_df$ry[j] > 0.6) & (final_df$cy[j] < 0.6)){
    ClassIV <- ClassIV +1
  } else if ((final_df$signal_code[j] ==  "cfp nuc rfp yfp") & 
             (final_df$ry[j] < 0.6)){
    ClassIII <- ClassIII +1 
  } else if ((final_df$signal_code[j] ==  "cfp cfp nuc rfp yfp")){
    Other3 <- Other3 +1 
  } else if ((final_df$signal_code[j] ==  "cfp nuc rfp rfp yfp")){
    Other4 <- Other4 +1 
  } else if ((final_df$signal_code[j] ==  "cfp nuc rfp yfp yfp")){
    ClassI <- ClassI +1 
  } else {
    unknow <- unknow +1
  }
}



datat_result[1,2] <- ClassI 
datat_result[2,2] <- ClassII 
datat_result[3,2] <- ClassIII 
datat_result[4,2] <- ClassIV 
datat_result[5,2] <- Other1
datat_result[6,2] <- Other2
datat_result[7,2] <- Other3
datat_result[8,2] <- Other4
datat_result[9,2] <- CFP2only
datat_result[10,2] <- onlyCFP
datat_result[11,2] <- onlyRFP 
datat_result[12,2] <- onlyYFP 
datat_result[13,2] <- noCFP 
datat_result[14,2] <- noRFP
datat_result[15,2] <- All3clumped
datat_result[16,2] <- All3separated 
datat_result[17,2] <- unknow

length(final_df[which(final_df$signal_code ==  "cfp nuc rfp"), ])
