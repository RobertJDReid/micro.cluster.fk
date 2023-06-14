
# Looking for precise signals in the dataframe final_df

# Looking for cfp, cfp, nuc, yfp :

# final_df[which(final_df$signal_code == "cfp cfp nuc yfp"), c('sample', 'field')]
final_df[which(final_df$signal_code == "cfp cfp nuc yfp"), ]


#Result
cfp_nuc_rfp_yfp <- c('c3_14_11', 'c3_16_17', 'c3_31_7', 'c3_5_8', 'c3_14_9', 'c3_14_10', 'c3_15_8', 
                     'c3_15_9', 'c3_16_15', 'c3_28_4', 'c3_30_6', 'c3_31_6', 'c3_32_5', 'c3_35_4', 
                     'c3_36_6', 'c3_37_4', 'c3_11_2', 'c3_14_6', 'c3_37_1', 'c3_14_3', 'c3_15_1', 'c3_15_5',
                     'c3_16_5', 'c3_38_5', 'c3_29_2', 'c3_24_4', 'c3_17_6', 'c3_25_13')
cfp_nuc <- c('c3_1_8', 'c3_1_10', 'c3_30_8', 'c3_31_8', 'c3_16_14', 'c3_33_9')
cfp_nuc_yfp <- c('c3_16_16', 'c3_36_10', 'c3_16_1', 'c3_6_3')
cfp_cfp_nuc_rfp <- c('c3_3_5')
cfp_nuc_rfp <- c('c3_28_5', 'c3_33_8', 'c3_9_7', 'c3_9_8')
cfp_nuc_yfp_yfp <- c('c3_33_1')
nuc_rfp_yfp <- c('c3_13_6')
cfp_cfp_nuc_rfp_yfp <- c('c3_1_5')

#Extra observation
budding <- c('c3_1_5', 'c3_1_10', 'c3_14_11', 'c3_28_5', 'c3_30_6', 'c3_35_4', 'c3_37_4', 'c3_9_7', 'c3_9_7', 'c3_14_6', 'c3_37_1', 
              'c3_33_1', 'c3_38_5')
vacuole <- c('c3_16_16', 'c3_3_5', 'c3_15_1', 'c3_16_1', 'c3_17_6', 'c3_25_13')

corner <- c('c3_1_5', 'c3_14_11', 'c3_16_17', 'c3_3_5', 'c3_30_8', 'c3_31_7', 'c3_33_9', 'c3_5_8', 'c3_14_9',
            'c3_28_4', 'c3_28_5', 'c3_30_6', 'c3_31_6', 'c3_37_4', 'c3_9_7', 'c3_9_8', 'c3_37_1',
            'c3_16_1', 'c3_25_13', 'c3_6_3')

modif_class <- c(cfp_nuc_rfp_yfp, cfp_nuc, cfp_nuc_yfp, 
                 cfp_cfp_nuc_rfp, cfp_nuc_rfp, cfp_nuc_yfp_yfp, nuc_rfp_yfp, cfp_cfp_nuc_rfp_yfp)







### Creation of new dataframe with corrections

final_df_corr <- final_df

final_df_corr$correction <- c("No")
x <- 0
y <- c()
for (i in 1:length(modif_class)) {
  if (modif_class[i] %in% final_df_corr$cluster_id) {
    x <- x +1
    y <- c(y, modif_class[i])
    if (modif_class[i] %in% cfp_nuc_rfp_yfp) {
      final_df_corr[which(final_df_corr$cluster_id %in% modif_class[i]), "signal_code"] <- 'cfp nuc rfp yfp' 
      final_df_corr[which(final_df_corr$cluster_id %in% modif_class[i]), "correction"] <-"yes"
      } 
    if (modif_class[i] %in% cfp_nuc) {
      final_df_corr[which(final_df_corr$cluster_id %in% modif_class[i]), "signal_code"] <- "cfp nuc" 
    }
    if (modif_class[i] %in% cfp_nuc_yfp) {
      final_df_corr[which(final_df_corr$cluster_id %in% modif_class[i]), "signal_code"] <- "cfp nuc yfp" 
    }
    if (modif_class[i] %in% cfp_cfp_nuc_rfp) {
      final_df_corr[which(final_df_corr$cluster_id %in% modif_class[i]), "signal_code"] <- "cfp cfp nuc rfp" 
    }
    if (modif_class[i] %in% cfp_nuc_rfp) {
      final_df_corr[which(final_df_corr$cluster_id %in% modif_class[i]), "signal_code"] <- "cfp nuc rfp" 
    }
    if (modif_class[i] %in% cfp_nuc_yfp_yfp) {
      final_df_corr[which(final_df_corr$cluster_id %in% modif_class[i]), "signal_code"] <- "cfp nuc yfp yfp" 
    }
    if (modif_class[i] %in% nuc_rfp_yfp) {
      final_df_corr[which(final_df_corr$cluster_id %in% modif_class[i]), "signal_code"] <- "nuc rfp yfp" 
    }
    if (modif_class[i] %in% cfp_cfp_nuc_rfp_yfp) {
      final_df_corr[which(final_df_corr$cluster_id %in% modif_class[i]), "signal_code"] <- "cfp cfp nuc rfp yfp" 
    }
  }
}




#Add budding yeast and vacuole

final_df_corr$observation <- c("No")
for (i in 1:length(budding)) {
  if ((budding[i] %in% final_df_corr$cluster_id)) {
      final_df_corr[which(final_df_corr$cluster_id %in% budding[i]), "observation"] <- "budding" 
  } 
  if (budding[i] %in% final_df_corr$cluster_id & (budding[i] %in% cfp_nuc_rfp_yfp)) {
    final_df_corr[which(final_df_corr$cluster_id %in% budding[i]), "observation"] <- "budding corrected" 
  }
}
for (i in 1:length(vacuole)) {
  if ((vacuole[i] %in% final_df_corr$cluster_id)) {
    final_df_corr[which(final_df_corr$cluster_id %in% vacuole[i]), "observation"] <- "vacuole" 
  } 
  if (vacuole[i] %in% final_df_corr$cluster_id & (vacuole[i] %in% cfp_nuc_rfp_yfp)) {
    final_df_corr[which(final_df_corr$cluster_id %in% vacuole[i]), "observation"] <- "vacuole corrected" 
  } 
}

final_df_corr$region <- c("No")
for (i in 1:length(corner)) {
  if ((corner[i] %in% final_df_corr$cluster_id)) {
    final_df_corr[which(final_df_corr$cluster_id %in% corner[i]), "region"] <- "yes" 
  } 
}
rm(i)


### Représentation graphique

final_df_corr %>%
  ggplot(aes(x=signal_code,y= cluster_id)) +
  geom_point(position = "jitter", aes(colour=observation)) +
  scale_colour_manual(values = c("dark green", "green", "black", "dark blue", "blue"))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.title = element_blank())

final_df_corr %>%
  ggplot(aes(x=signal_code,y= cluster_id)) +
  geom_point(position = "jitter", aes(colour=correction)) +
  ggtitle("Graph highlighting corrected microscopy images with eyes ")+
  scale_colour_manual(values = c("black", "green"))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.title = element_blank())

final_df_corr %>%
  ggplot(aes(x=signal_code,y= cluster_id)) +
  geom_point(position = "jitter", aes(colour=region)) +
  ggtitle("Graph highlighting yeasts in microscopy images corners")+
  scale_colour_manual(values = c("black", "green"))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.title = element_blank())

final_df %>%
  ggplot(aes(x=signal_code,y= cluster_id)) +
  geom_point(position = "jitter") +
  #scale_colour_manual(values = c("blue", "black","yellow"))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.title = element_blank())




### On data corrected

x <- 0
min_cenZ <- c()
max_cenZ <- c()
nb_tot <- c()
nb_normal <-c()
nb_out <-c()
rm(i)
  for (i in seq(0,0.7,0.1)) {

     x <- x+1
     final <- data.frame()
   
    inbound_nuclei <- cluster_df %>%
      filter(str_detect(xfp_id,"nucleus")) %>%
      left_join(full_data) %>%
      filter(cenZ >= 1.5 + i & cenZ <= 3.3 - i) %>%
      filter(cenX >= 1.5 & cenX <= 85) %>%
      filter(cenY >= 1.5 & cenY <= 64) %>%
      select(sample,field,cluster_number) %>%
      unite("cluster_id",c(sample,field,cluster_number),remove = FALSE)
    
      RYdf <- map(cluster_set_dist,function(m) {
      
      y <- grep("yf",rownames(m),value = TRUE)
      r <- grep("rf",rownames(m),value = TRUE)
      c <- grep("cf",colnames(m),value = TRUE)
      
      if (length(y)+length(r)+length(c) > 3 |
          length(y)+length(r)+length(c) < 2) {
        rval <- c(NA,NA,NA)
        names(rval) <- c("ry","cy","cr")
        return(rval)
      } else {
        if(is_empty(r) | is_empty(y)) ry=NA
        else ry = min(m[r,y])
        if(is_empty(c) | is_empty(y)) cy=NA
        else cy = min(m[c,y])
        if(is_empty(c) | is_empty(r)) cr=NA
        else cr = min(m[c,r])
        rval <- c(ry,cy,cr)
        names(rval) <- c("ry","cy","cr")
        return(rval)
      }
    }) %>%
      as_tibble() %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column("sample.field.group") %>%
      rename(c("ry"=V1,"cy"=V2,"cr"=V3)) %>%
      separate(col=sample.field.group,into = c("sample","field","cluster_number"),sep = "\\.") %>%
      mutate(cluster_number = as.integer(cluster_number))
    
    final_df <- inbound_nuclei %>%
      left_join(cluster_signals_df) %>%
      left_join(cluster_pos) %>%
      left_join(RYdf)

    for (j in 1:nrow(final_df_corr)) {
      if (final_df_corr$cluster_id[j] %in% final_df$cluster_id) {
        final <- rbind(final, final_df_corr[j, ])
      }
    }
    min_cenZ[x] <- 1.5 + i
    max_cenZ[x] <- 3.3 - i
    
    final %>%
      ggplot(aes(x=signal_code,y= cluster_id)) +
      geom_point(position = "jitter", aes(colour=correction)) +
      ggtitle(paste("Graph highlighting corrected microscopy images with eyes & ", min_cenZ[x], "<cen(Z)<", max_cenZ[x]))+
      scale_colour_manual(values = c("black", "green"))+
      theme(axis.text.x = element_text(angle = 45,hjust = 1), 
            axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      theme(legend.title = element_blank())
    ggsave(paste("graph : ", min_cenZ[x], "<cen(Z)<", max_cenZ[x], ".png"))
    
    nb_tot[x] <- nrow(final)
    nb_normal[x] <- length(which(final$signal_code == 'cfp nuc rfp yfp'))
    nb_out[x] <- length(which(final$signal_code != 'cfp nuc rfp yfp'))
    save(final, file =paste("Final dataframe : ", min_cenZ[x], "< cen(Z) <", max_cenZ[x],".Rda" ))
    rm(final)
}
df_recap_corr <- rbind(min_cenZ, max_cenZ, nb_tot, nb_normal, nb_out)


### On data not corrected

x <- 0
min_cenZ <- c()
max_cenZ <- c()
nb_tot <- c()
nb_normal <-c()
nb_out <-c()
rm(i)
for (i in seq(0,0.7,0.1)) {
  
  x <- x+1
  final <- data.frame()
  
  inbound_nuclei <- cluster_df %>%
    filter(str_detect(xfp_id,"nucleus")) %>%
    left_join(full_data) %>%
    filter(cenZ >= 1.5 + i & cenZ <= 3.3 - i) %>%
    filter(cenX >= 1.5 & cenX <= 85) %>%
    filter(cenY >= 1.5 & cenY <= 64) %>%
    select(sample,field,cluster_number) %>%
    unite("cluster_id",c(sample,field,cluster_number),remove = FALSE)
  
  RYdf <- map(cluster_set_dist,function(m) {
    
    y <- grep("yf",rownames(m),value = TRUE)
    r <- grep("rf",rownames(m),value = TRUE)
    c <- grep("cf",colnames(m),value = TRUE)
    
    if (length(y)+length(r)+length(c) > 3 |
        length(y)+length(r)+length(c) < 2) {
      rval <- c(NA,NA,NA)
      names(rval) <- c("ry","cy","cr")
      return(rval)
    } else {
      if(is_empty(r) | is_empty(y)) ry=NA
      else ry = min(m[r,y])
      if(is_empty(c) | is_empty(y)) cy=NA
      else cy = min(m[c,y])
      if(is_empty(c) | is_empty(r)) cr=NA
      else cr = min(m[c,r])
      rval <- c(ry,cy,cr)
      names(rval) <- c("ry","cy","cr")
      return(rval)
    }
  }) %>%
    as_tibble() %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("sample.field.group") %>%
    rename(c("ry"=V1,"cy"=V2,"cr"=V3)) %>%
    separate(col=sample.field.group,into = c("sample","field","cluster_number"),sep = "\\.") %>%
    mutate(cluster_number = as.integer(cluster_number))
  
  final_df <- inbound_nuclei %>%
    left_join(cluster_signals_df) %>%
    left_join(cluster_pos) %>%
    left_join(RYdf)
  
  min_cenZ[x] <- 1.5 + i
  max_cenZ[x] <- 3.3 - i
  
  final_df %>%
    ggplot(aes(x=signal_code,y= cluster_id)) +
    geom_point(position = "jitter") +
    ggtitle(paste("Graph highlighting ", min_cenZ[x], "<cen(Z)<", max_cenZ[x]))+
    theme(axis.text.x = element_text(angle = 45,hjust = 1), 
          axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme(legend.title = element_blank())
  ggsave(paste("graph : ", min_cenZ[x], "<cen(Z)<", max_cenZ[x], ".png"))
  
  nb_tot[x] <- nrow(final_df)
  nb_normal[x] <- length(which(final_df$signal_code == 'cfp nuc rfp yfp'))
  nb_out[x] <- length(which(final_df$signal_code != 'cfp nuc rfp yfp'))
  save(final, file =paste("Final dataframe : ", min_cenZ[x], "< cen(Z) <", max_cenZ[x],".Rda" ))
  rm(final)
}
df_recap_not_corr <- rbind(min_cenZ, max_cenZ, nb_tot, nb_normal, nb_out)

