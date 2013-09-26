#' Plot proportion of errors per item in the IAT
#' 
#' This function uses ggplot2 to plot proportion of errors in the IAT to see if any items yield high error rates. The data is automatically cleaned
#' (i.e., trials with RT > 10000 or < 180 are deleted) to avoid over/underinflation of mean estimates and only include trials from
#' essential blocks.
#' 
#' @param myData The raw dataframe to be used
#' @param blockName A string of the variable name for the blocks
#' @param trialBlocks A vector of the four essential blocks in the seven-block IAT (i.e., B3, B4, B6, and B7).
#' @param sessionID A string of the variable name identifying each unique participant.
#' @param itemName A string of the variable identifying the items
#' @param trialLatency A string of the variable name for the latency of each trial.
#' @param trialError A string of the variable name identifying whether a trial was an error or not (1 = error)
#' @import ggplot2 data.table
#' @export

plotItemErr <- function(myData, blockName, trialBlocks, sessionID, itemName, trialLatency, trialError){
  
  # to appease global variable check
  Item <- NULL; propErrors <- NULL
  
  myDataTable <- data.table(myData)
  
  byItemErr <- myDataTable[get(trialLatency) > 180 & get(trialLatency) < 10000,
                           list(propErrors = sum(get(trialError) == 1)/length(get(trialError))),
                           by = list(Item = get(itemName))]
  
  byItemErr <- as.data.frame(byItemErr)
  
  byItemErr$Item <- factor(byItemErr$Item, levels = byItemErr[order(byItemErr$propErrors),]$Item)
  
  myMean <- mean(byItemErr$propErrors)
  
  p <- ggplot(byItemErr, aes(x = Item, y = propErrors)) + geom_bar(fill = "dark gray") + geom_hline(aes(yintercept = mean(propErrors)), size = 1.5)
  p <- p + coord_flip() + theme_bw() 
  suppressMessages(suppressWarnings(print(p)))
  
}