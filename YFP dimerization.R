## YFP dimerization

# Class I
cr_distance_classI <- final_df[, c('cluster_id', 'cr')]
cr_distance_classI <- rbind(cr_distance_classI, final_df[, c('cluster_id', 'cr')])

cr_distance_classI <- cr_distance_classI[-which(is.na(cr_distance_classI$cr)), ]

ggplot(cr_distance_classI, aes(x=cr)) + geom_density()

save(cr_distance_classI, file = "distance_classI.RData")


# Uncut
cr_distance_uncut <- rbind(cr_distance_uncut, final_df[, c('cluster_id', 'cr')])
cr_distance_uncut <- final_df[, c('cluster_id', 'cr')]

cr_distance_uncut <- cr_distance_uncut[-which(is.na(cr_distance_uncut$cr)), ]

ggplot(cr_distance_uncut, aes(x=cr)) + geom_density()

save(cr_distance_uncut, file = "distance_uncut.RData")




#############################################################


ggplot(final_df) + 
  geom_density(aes(x=cr, color = 'blue')) +
  geom_density(aes(x=ry, color = 'red')) +
  geom_density(aes(x=cy, color = 'yellow')) +
  geom_vline(xintercept = 0.62)
 



ggplot(final_df, aes(x=cr)) + geom_density()













