#' Plot intraindividual variability of reaction time
#' 
#' This function uses ggplot2 to plot intraindividual variability in reaction time, faceted by the four essential blocks. 
#' 
#' @param myData The raw dataframe to be used
#' @param dataType A string of "raw" for no cleaning, or "clean" for cleaned data (no error trials, RT < 10,000ms, and RT > 180ms)
#' @param blockName A string of the variable name for the blocks
#' @param trialBlocks A vector of the four essential blocks in the seven-block IAT (i.e., B3, B4, B6, and B7).
#' @param sessionID A string of the variable name identifying each unique participant.
#' @param trialNumber A string of the variable identifying the trial number.
#' @param trialLatency A String of the variable name for the latency of each trial.
#' @import ggplot2
#' @export

plotIIV <- function(myData, dataType, blockName, trialBlocks, sessionID, trialNumber, trialLatency){
  
  if(length(unique(myData[, sessionID])) > 100){
    
    sampleIDs <- sample(unique(myData[, sessionID]), 100)
    myData <- myData[myData[, sessionID] %in% sampleIDs, ]
    warning("Your total sample size is > 100. A random subsample was taken for plotting.")
    
  }
  
  if(dataType == "clean"){
    
    myDataNew <- myData[myData[, blockName] %in% trialBlocks, ]
    
    myDataNew <- myDataNew[myDataNew[, trialLatency] > 180 & myDataNew[, trialLatency] < 10000, ]
    
  } else if(dataType == "raw"){
    
    myDataNew <- myData[myData[, blockName] %in% trialBlocks, ]
    
  } else stop("Please enter 'raw' or 'clean' for dataType.")
  
  myDataNew[, blockName] <- as.factor(myDataNew[, blockName])
  myDataNew[, trialNumber] <- as.factor(myDataNew[, trialNumber])
  
  stat_sum_single <- function(fun, geom ="point", colour = "black", size = 1.5, ...) {
    stat_summary(fun.y=fun, colour = colour, geom = geom, size = size, ...)
  }
  
  p <- ggplot(myDataNew, aes_string(x = trialNumber, y = trialLatency)) + geom_line(aes_string(group = sessionID), colour = "#999999")
  p <- p + theme_bw() + stat_sum_single(mean, geom = "line", aes(group = 1), size = 1.5, colour = "black") + theme_bw()
  p <- p + facet_grid(paste(". ~ ", blockName, sep = "")) + theme(axis.text.x = element_blank())
  p
  
}
