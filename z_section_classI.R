### New z-section : work on class 1

# c1_5_3 à supprimeer

cfp_nuc_rfp_yfp_yfp <- c('c1_1_4', 'c1_1_6', 'c1_1_5', 'c1_1_2', 'c1_2_5', 'c1_2_2', 'c1_2_6', 'c1_2_7',  'c1_2_1',
                         'c1_2_3',  'c1_3_11',  'c1_3_7', 'c1_3_5',  'c1_3_9', 'c1_3_8', 'c1_3_6', 'c1_3_2', 'c1_3_1',
                         'c1_3_3',  'c1_5_7', 'c1_5_9', 'c1_5_6', 'c1_5_1', 'c1_5_5', 'c1_5_2', 'c1_5_4', 'c1_5_8',
                         'c4_2_8', 'c4_2_6', 'c4_2_7', 'c4_3_3', 'c4_3_4', 'c4_3_2')

nuc_rfp_yfp <- c('c1_1_7', 'c4_2_4')

cfp_nuc_rfp_yfp <- c('c4_2_2')

modif_class <- c(cfp_nuc_rfp_yfp_yfp, nuc_rfp_yfp, cfp_nuc_rfp_yfp)

final_df$correction <- c("No")

for (i in 1:length(modif_class)) {
  if (modif_class[i] %in% final_df$cluster_id) {
    if (modif_class[i] %in% cfp_nuc_rfp_yfp_yfp) {
      final_df[which(final_df$cluster_id %in% modif_class[i]), "signal_code"] <- 'cfp nuc rfp yfp yfp' 
      final_df[which(final_df$cluster_id %in% modif_class[i]), "correction"] <-"yes"
    } 
    if (modif_class[i] %in% nuc_rfp_yfp) {
      final_df[which(final_df$cluster_id %in% modif_class[i]), "signal_code"] <- "nuc rfp yfp" 
      final_df[which(final_df$cluster_id %in% modif_class[i]), "correction"] <-"yes"
    }
    if (modif_class[i] %in% cfp_nuc_rfp_yfp) {
      final_df[which(final_df$cluster_id %in% modif_class[i]), "signal_code"] <- "cfp nuc rfp yfp"
      final_df[which(final_df$cluster_id %in% modif_class[i]), "correction"] <-"yes"
    }
  }
}