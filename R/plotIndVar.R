#' Plot individual variability in the IAT
#' 
#' This function uses ggplot2 to plot mean participant reaction time with 95\% confidence intervals to see how reaction time varies by participant. The data is automatically cleaned
#' (i.e., no error trials, trials with RT > 10000 or < 180 are deleted) to avoid over/underinflation of mean estimates and only include trials from
#' essential blocks.
#' 
#' @param myData The raw dataframe to be used
#' @param blockName A string of the variable name for the blocks
#' @param trialBlocks A vector of the four essential blocks in the seven-block IAT (i.e., B3, B4, B6, and B7).
#' @param sessionID A string of the variable name identifying each unique participant.
#' @param trialLatency A string of the variable name for the latency of each trial.
#' @param trialError A string of the variable name identifying whether a trial was an error or not (1 = error)
#' @import ggplot2 data.table
#' @export

plotIndVar <- function(myData, blockName, trialBlocks, sessionID, trialLatency, trialError){
  
  # To appease global variable check
  ID <- NULL; meanRT <- NULL; mySE <- NULL
  
  if(length(unique(myData[, sessionID])) > 100){
    
    sampleIDs <- sample(unique(myData[, sessionID]), 100)
    myData <- myData[myData[, sessionID] %in% sampleIDs, ]
    warning("Your total sample size is > 100. A random subsample was taken for plotting.")
    
  }
  
  myDataTable <- data.table(myData)
  
  byPart <- myDataTable[get(trialLatency) > 180 & get(trialLatency) < 10000 & get(trialError) == 0 & get(blockName) %in% trialBlocks,
                        list(N = length(get(trialLatency)),
                             meanRT = mean(get(trialLatency)),
                             mySD = sd(get(trialLatency)),
                             mySE = sd(get(trialLatency))/sqrt(length(get(trialLatency)))),
                        by = list(ID = get(sessionID))]
  
  byPart <- as.data.frame(byPart)
  
  byPart$ID <- factor(byPart$ID, levels = byPart[order(byPart$meanRT),]$ID)
  
  p <- ggplot(byPart, aes(x = ID, y = meanRT)) + geom_errorbar(aes(ymin = meanRT - 2*mySE, ymax = meanRT + 2*mySE), width = 0)
  p <- p + geom_line() + geom_point() + coord_flip() + theme_bw()
  suppressMessages(print(p))
  
}
