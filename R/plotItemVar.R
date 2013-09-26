#' Plot IAT item variability
#' 
#' This function uses ggplot2 to plot mean item reaction time with 95\% confidence intervals to see how reaction time varies by item. The data is automatically cleaned
#' (i.e., no error trials, and trials with RT > 10000 or < 180 are deleted) to avoid over/underinflation of mean estimates and only include trials from essential blocks.
#' 
#' @param myData The raw dataframe to be used
#' @param blockName A string of the variable name for the blocks
#' @param trialBlocks A vector of the four essential blocks in the seven-block IAT (i.e., B3, B4, B6, and B7).
#' @param sessionID A string of the variable name identifying each unique participant.
#' @param itemName A string of the variable identifying the items
#' @param trialLatency A string of the variable name for the latency of each trial.
#' @param trialError A string of the variable name identifying whether a trial was an error or not (1 = error)
#' @import data.table ggplot2
#' @export

plotItemVar <- function(myData, blockName, trialBlocks, sessionID, itemName, trialLatency, trialError){
  
  # to appease global variable checks
  Item <- NULL; meanRT <- NULL; mySE <- NULL
  
  myDataTable <- data.table(myData)
  
  byItem <- myDataTable[get(trialLatency) > 180 & get(trialLatency) < 10000 & get(trialError) == 0 & get(blockName) %in% trialBlocks,
                        list(N = length(get(trialLatency)),
                             meanRT = mean(get(trialLatency)),
                             mySD = sd(get(trialLatency)),
                             mySE = sd(get(trialLatency))/sqrt(length(get(trialLatency)))),
                        by = list(Item = get(itemName))]
  
  byItem <- as.data.frame(byItem)
  
  byItem$Item <- factor(byItem$Item, levels = byItem[order(byItem$meanRT), ]$Item)
  
  p <- ggplot(byItem, aes(x = Item, y = meanRT)) + geom_errorbar(aes(ymin = meanRT - 2*mySE, ymax = meanRT + 2*mySE), width = 0)
  p <- p + geom_line() + geom_point() + coord_flip() + theme_bw()
  suppressMessages(print(p))
  
}
