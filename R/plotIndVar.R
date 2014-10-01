#' Plot individual variability in the IAT
#' 
#' This function uses ggplot2 to plot mean participant reaction time with 95\% confidence intervals to see how reaction time varies by participant. The data is automatically cleaned
#' (i.e., no error trials, trials with RT > 10000 or < 180 are deleted) to avoid over/underinflation of mean estimates and only include trials from
#' essential blocks.
#' 
#' @param myData The raw dataframe to be used
#' @param sessionID A string of the variable name identifying each unique participant.
#' @param blockName A string of the variable name for the blocks
#' @param trialBlocks A vector of the four essential blocks in the seven-block IAT (i.e., B3, B4, B6, and B7).
#' @param trialLatency A string of the variable name for the latency of each trial.
#' @param trialError A string of the variable name identifying whether a trial was an error or not (1 = error)
#' @import ggplot2 dplyr
#' @export

plotIndVar <- function(myData, blockName, trialBlocks, sessionID, trialLatency, trialError){
  
  # To appease global variable check
  SESSION_ID <- NULL; TRIAL_LATENCY <- NULL; TRIAL_ERROR <- NULL; BLOCK_NAME <- NULL; meanRT <- NULL; mySE <- NULL
  
  names(myData)[names(myData) == trialLatency] <- "TRIAL_LATENCY"
  names(myData)[names(myData) == trialError] <- "TRIAL_ERROR"
  names(myData)[names(myData) == blockName] <- "BLOCK_NAME"
  names(myData)[names(myData) == sessionID] <- "SESSION_ID"

  if(length(unique(myData[, sessionID])) > 100){
    
    sampleIDs <- sample(unique(myData[, sessionID]), 100)
    myData <- myData[myData[, sessionID] %in% sampleIDs, ]
    warning("Your total sample size is > 100. A random subsample was taken for plotting.")
    
  }
  
  myTbl <- group_by(tbl_df(myData), SESSION_ID)

  byPart <- filter(myTbl, TRIAL_LATENCY > 180 & TRIAL_LATENCY < 10000, TRIAL_ERROR == 0, BLOCK_NAME %in% trialBlocks) %>%
    summarise(N = length(TRIAL_LATENCY),
              meanRT = mean(TRIAL_LATENCY),
              mySD = sd(TRIAL_LATENCY),
              mySE = sd(TRIAL_LATENCY)/sqrt(length(TRIAL_LATENCY)))
  
  byPart$SESSION_ID <- factor(byPart$SESSION_ID, levels = byPart[order(byPart$meanRT),]$SESSION_ID)

  p <- ggplot(byPart, aes(x = SESSION_ID, y = meanRT)) + geom_errorbar(aes(ymin = meanRT - 2*mySE, ymax = meanRT + 2*mySE), width = 0)
  p <- p + geom_line() + geom_point() + coord_flip() + theme_bw() + labs(x = "Session ID", y = "Mean Reaction Time")
  suppressMessages(print(p))
  
}
