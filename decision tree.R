### Decision tree

library(stringi)


decision <- function(data, i) {

myResult <-  
  if ("cfp nuc rfp yfp" == data$signal_code[i]) { # IL Y A LES 3 COULEURS
    if (stri_count_fixed(data$signal_code[i], 'yfp') == 2 |
        stri_count_fixed(data$signal_code[i], 'rfp') == 2 |
        stri_count_fixed(data$signal_code[i], 'cfp') == 2) {
      if (stri_count_fixed(data$signal_code[i], 'yfp') == 2 ) {
        return("Class I") 
      } else if (stri_count_fixed(data$signal_code[i], 'rfp') == 2 ) {
        return("Other 4")
      } else {
        return("Other 3") }
    } else if (data$ry[i] < 0.6) {
      return("Class III") 
    } else if (data$cy[i] < 0.6) {
      return("Class IV") 
    } else if (data$cr[i] < 0.6) {
      return("All 3 clumped")
    } else {
      return("All 3 separated") }
 
  } else { # NON AU 3 COULEURS
    
      if ((lengths(gregexpr("\\W+", data$signal_code[i])) + 1) == 2) {
          if ((grep("cfp", data$signal_code[i]) == 1) & stri_count_fixed(data$signal_code[i], 'cfp') == 2) {
            return("2 CFP only") } 
          if (grep("cfp", data$signal_code[i]) == 1) {
            return("only CFP") }
          } else {
            return(data$signal_code[i]) }
      }
     
      if ((lengths(gregexpr("\\W+", data$signal_code[i])) + 1) == 3) {    
          if (grep("yfp", data$signal_code[i]) == 1){
            if (grep("rfp", data$signal_code[i]) == 1) {
              return("Other 2")
            } else { 
              return("no cfp") }
          } else {
            if (data$cr[i] < 0.6) {
              return("Other 1")
            } else {
              return("Class II") }
          }
      }
return(myResult)}

decision(final_df, 1)

decision(final_df, 8)
