#####################################
# Final decision tree

final_df <- final_df %>% mutate( decision = case_when((final_df$signal_code ==  "nuc rfp") ~ 'only RFP',
                                            (final_df$signal_code ==  "cfp nuc") ~ 'only CFP',
                                            (final_df$signal_code ==  "cfp cfp nuc") ~ '2 CFP only',
                                            (final_df$signal_code ==  "cfp nuc rfp" & is.na(final_df$cr)) ~ 'problem',
                                            (final_df$signal_code ==  "cfp nuc rfp" & final_df$cr > 0.6) ~ 'Class II',
                                            (final_df$signal_code ==  "cfp nuc rfp" & final_df$cr < 0.6) ~ 'Other 1', 
                                            (final_df$signal_code ==  "cfp nuc rfp") ~ 'Other 2',
                                            TRUE ~ 'unknow'))


decision_tree_result <- data.frame(name=c('Class I', 'Class II', 'Class III', 'Class IV', 'Other 1', 'Other 2', 'Other 3', 
                                 'Other 4', '2 CFP only', 'only CFP','only RFP', 'only YFP', 'no CFP', 'noRFP', 'All 3 clumped', 'All 3 separated', 'unknow'),
                          number=rep(0,17))

decision_tree_result[1,2] <- nrow(final_df[which(final_df$decision =='Class I'), ])
decision_tree_result[2,2] <- nrow(final_df[which(final_df$decision =='Class II'), ]) 
decision_tree_result[3,2] <- nrow(final_df[which(final_df$decision =='Class III' ), ])
decision_tree_result[4,2] <- nrow(final_df[which(final_df$decision =='Class IV'), ])
decision_tree_result[5,2] <- nrow(final_df[which(final_df$decision =='Other 1'), ])
decision_tree_result[6,2] <- nrow(final_df[which(final_df$decision =='Other 2'), ])
decision_tree_result[7,2] <- nrow(final_df[which(final_df$decision =='Other 3'), ])
decision_tree_result[8,2] <- nrow(final_df[which(final_df$decision =='Other 4'), ])
decision_tree_result[9,2] <- nrow(final_df[which(final_df$decision =='2 CFP only'), ])
decision_tree_result[10,2] <- nrow(final_df[which(final_df$decision =='only CFP'), ])
decision_tree_result[11,2] <- nrow(final_df[which(final_df$decision =='only RFP' ), ])
decision_tree_result[12,2] <- nrow(final_df[which(final_df$decision =='only YFP' ), ])
decision_tree_result[13,2] <- nrow(final_df[which(final_df$decision =='no CFP' ), ])
decision_tree_result[14,2] <- nrow(final_df[which(final_df$decision =='no RFP'), ])
decision_tree_result[15,2] <- nrow(final_df[which(final_df$decision =='All 3 clumped'), ])
decision_tree_result[16,2] <- nrow(final_df[which(final_df$decision =='All 3 separated'), ])
decision_tree_result[17,2] <- nrow(final_df[which(final_df$decision =='unknow'), ])







