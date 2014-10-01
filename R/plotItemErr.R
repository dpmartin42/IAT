#' Plot proportion of errors per item in the IAT
#' 
#' This function uses ggplot2 to plot proportion of errors in the IAT to see if any items yield high error rates. The data is automatically cleaned
#' (i.e., trials with RT > 10000 or < 180 are deleted) to avoid over/underinflation of mean estimates and only include trials from
#' essential blocks.
#' 
#' @param myData The raw dataframe to be used
#' @param sessionID A string of the variable name identifying each unique participant.
#' @param itemName A string of the variable identifying the items
#' @param trialLatency A string of the variable name for the latency of each trial.
#' @param trialError A string of the variable name identifying whether a trial was an error or not (1 = error)
#' @import ggplot2 dplyr
#' @export

plotItemErr <- function(myData, sessionID, itemName, trialLatency, trialError){

  # to appease global variable check
  ITEM <- NULL; propErrors <- NULL; TRIAL_LATENCY <- NULL; TRIAL_ERROR <- NULL
  
  names(myData)[names(myData) == trialLatency] <- "TRIAL_LATENCY"
  names(myData)[names(myData) == trialError] <- "TRIAL_ERROR"
  names(myData)[names(myData) == sessionID] <- "SESSION_ID"
  names(myData)[names(myData) == itemName] <- "ITEM"
  
  myTbl <- group_by(tbl_df(myData), ITEM)
  
  byItemErr <- filter(myTbl, TRIAL_LATENCY > 180 & TRIAL_LATENCY < 10000) %>%
    summarise(propErrors = sum(TRIAL_ERROR == 1)/length(TRIAL_ERROR))
  
  byItemErr$ITEM <- factor(byItemErr$ITEM, levels = byItemErr[order(byItemErr$propErrors),]$ITEM)
  
  myMean <- mean(byItemErr$propErrors)
  
  p <- ggplot(byItemErr, aes(x = ITEM, y = propErrors)) + geom_bar(fill = "dark gray", stat = "identity")
  p <- p + geom_hline(aes(yintercept = mean(propErrors)), size = 1.5)
  p <- p + coord_flip() + theme_bw() + labs(x = "Item", y = "Proportion Errors")
  suppressMessages(suppressWarnings(print(p)))
  
}