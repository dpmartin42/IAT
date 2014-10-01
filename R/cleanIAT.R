#' Clean IAT data using the updated D-Scoring algorithm
#' 
#' This macro will transform a dataframe with trial latencies (stored as one line per trial) 
#' for a standard format IAT (7 blocks) into a one line summary per subject of the IAT effect using
#' GNB's new scoring algorithm. The goal of this functionis to prepare IAT data for subsequent analysis.
#' However, this does not relieve the researcher from making conceptual decisions about how best to
#' analyze IAT data.  There are decisions to make about how the function is applied, and the function
#' does not remove participants.  All subject exclusions must be made deliberately by the researcher.
#' 
#' @param myData The raw dataframe to be used
#' @param blockName A string of the variable name for the blocks
#' @param trialBlocks A vector of the four essential blocks in the seven-block IAT (i.e., B3, B4, B6, and B7).
#' @param sessionID A string of the variable name identifying each unique participant.
#' @param trialLatency A string of the variable name for the latency of each trial.
#' @param trialError A string of the variable name identifying whether a trial was an error or not, where 1 indicates an error.
#' @param vError If 1 (current standard), then means are calculated for the entire set of latencies. If 2, error latencies will be replaced by the block mean + 600ms
#' @param vExtreme If 1, then no extreme value treatment. If 2 (current standard), delete trial latencies < 400ms
#' @param vStd If 1 (current standard), block SD is performed including error trials (corrected or not). If 2, block SD is performed on correct responses only
#' @return Outputs a dataframe that must be saved to an object. The variable IAT is the calculated D-Score for each individual. SUBEXCL notes
#' any exclusion criteria, with 0 being inclusion data, 1 for exclusion due to fast response, and 2 for exclusion due to missing blocks. C indicates
#' standard deviation for combined blocks (correct trial only), while A indicates standard deviations for combined blocks (all trials). M (mean), E (percent error),
#' N (number of trials used), and F (percent fast responses), are reported for each block included in the original dataframe. 
#' @references \href{http://faculty.washington.edu/agg/pdf/GB&N.JPSP.2003.pdf}{Understanding and Using the Implicit Association Test: I. An Improved Scoring Algorithm}
#' @examples
#' # Get Ps who recieve Math-Male sorting task in first blocks
#' 
#' myDataCongFirst <- IATData[IATData$isCongruentFirst == 1, ]
#' 
#' myDScoreCongFirst <- cleanIAT(myData = myDataCongFirst,
#'                               blockName = "BLOCK_NAME_S",
#'                               trialBlocks = c("BLOCK2", "BLOCK3", "BLOCK5", "BLOCK6"),
#'                               sessionID = "SESSION_ID",
#'                               trialLatency = "TRIAL_LATENCY",
#'                               trialError = "TRIAL_ERROR",
#'                               vError = 1, vExtreme = 2, vStd = 1)
#' 
#' # Get Ps who recieve Math-Female sorting task in first blocks
#' 
#' myDataCongSec <- IATData[IATData$isCongruentFirst == 0, ]
#' 
#' myDScoreCongSec <- cleanIAT(myData = myDataCongSec,
#'                             blockName = "BLOCK_NAME_S",
#'                             trialBlocks = c("BLOCK2", "BLOCK3", "BLOCK5", "BLOCK6"),
#'                             sessionID = "SESSION_ID",
#'                             trialLatency = "TRIAL_LATENCY",
#'                             trialError = "TRIAL_ERROR",
#'                             vError = 1, vExtreme = 2, vStd = 1)
#' 
#' myDScore <- rbind(myDScoreCongFirst, myDScoreCongSec)
#'                             
#' @import dplyr
#' @export

cleanIAT <- function(myData, blockName, trialBlocks, sessionID, trialLatency, trialError, vError, vExtreme, vStd){
  
  # To appease global variable check
  SESSION_ID <- NULL; TRIAL_LATENCY <- NULL; BLOCK_NAME <- NULL; total <- NULL; TRIAL_ERROR <- NULL;
  blockPairs <- NULL; MBLOCK2 <- NULL; MBLOCK3 <- NULL; MBLOCK5 <- NULL; MBLOCK6 <- NULL; 
  FASTM <- NULL; SUBEXCL <- NULL; myBlockMean <- NULL; DIFF1 <- NULL; DIFF2 <- NULL;
  IAT <- NULL; IAT1 <- NULL; IAT2 <- NULL; AS1 <- NULL; AS2 <- NULL; CS1 <- NULL; CS2 <- NULL
  
  # Rename variables to pass to dplyr functions
  
  names(myData)[names(myData) == blockName] <- "BLOCK_NAME"
  names(myData)[names(myData) == sessionID] <- "SESSION_ID"
  names(myData)[names(myData) == trialLatency] <- "TRIAL_LATENCY"
  
  myTbl <- group_by(tbl_df(myData), SESSION_ID)
  
  myTbl$SUBEXCL <- 0
  
  # Step 1: Include data from B3, B4, B6, B7
  # Step 1 has been removed so all data is at least partially analyzed
  
  # Step 2a: Eliminate trial latencies > 10,000ms and < 0ms
  
  myTblNoLong <- filter(myTbl, TRIAL_LATENCY < 10000, TRIAL_LATENCY >= 0)
  
  # Step 2b: Mark subjects for whom more than 10% of trials have latencies < 300ms with a 1
  
  myFastTbl <- filter(myTbl, BLOCK_NAME %in% trialBlocks) %>%
    summarise(FASTM = sum(TRIAL_LATENCY < 300)/length(TRIAL_LATENCY))
  
  isTooFast <- filter(myFastTbl, FASTM > 0.10) %>%
    select(SESSION_ID)
  
  if(nrow(isTooFast) > 0){
    
    myTbl[myTbl$SESSION_ID %in% isTooFast, ]$SUBEXCL <- 1
    
  }
  
  # Step 2c: Mark subjects with any missing blocks with a 2
  
  numBlocks <- filter(myTbl, BLOCK_NAME %in% trialBlocks) %>%
    summarise(total = length(unique(BLOCK_NAME)))
  
  notComplete <- filter(numBlocks, total < 4)
  
  if(nrow(notComplete) > 0){
    
    myTbl[myTbl$SESSION_ID %in% notComplete, ]$SUBEXCL <- 2
    
  }
  
  # Step 3: Use all trials (in the conventional algorithm) the first two trials of each
  # block would be dropped here
  
  # Step 4: No extreme value treatment <or> delete trial with latencies < 400ms
  
  if(vExtreme == 1){
    
    myTblNotFast <- group_by(myTblNoLong, SESSION_ID, BLOCK_NAME)
    
  } else if(vExtreme == 2){
    
    myTblNotFast <- group_by(filter(myTblNoLong, TRIAL_LATENCY >= 400), SESSION_ID, BLOCK_NAME)
    
  }
  
  # Step 5: Compute mean of correct latencies for each block
  # If vError = 1 then means and SDs will be calculated for the entire set of latencies
  # IF vError = 2 then the algorithm will replace error trial latencies with blockmean + 600
  # (where blockmean is mean of correct responses only)
  
  # Step 5: Compute mean of correct latencies for each block
  # If vError = 1 then means and SDs will be calculated for the entire set of latencies
  # IF vError = 2 then the algorithm will replace error trial latencies with blockmean + 600
  # (where blockmean is mean of correct responses only)
  
  if(vError == 1){
    
    blockMeans <- summarise(myTblNotFast, M = mean(TRIAL_LATENCY), N = length(TRIAL_LATENCY))
    
  } else if(vError == 2){
    
    meanReplace <- filter(myTblNotFast, TRIAL_ERROR == 0) %>%
      summarise(blockMean = mean(TRIAL_LATENCY) + 600)
    
    mergeTbl <- merge(myTblNotFast, meanReplace, by = c(sessionID, blockName), all = TRUE)
    
    myTblNotFast$tmpLatency <- myTblNotFast$TRIAL_LATENCY
    
    names(myTblNotFast)[c(14, 18)] <- c("oldLatency", trialLatency)
    
    blockMeans <- summarise(myTblNotFast, M = mean(TRIAL_LATENCY), N = length(TRIAL_LATENCY))
    
  } else stop("Please pick a value of 1 or 2 for vError.")
  
  # Step 6: Compute pooled SD for B3 & B6, and separately for B4 & B7.
  # If vStd is 1, the block SD is performed including error trials (corrected or not)
  # If vStd is 2, the block SD is performed on correct responses only
  
  myTblNotFast$blockPairs <- as.character(myTblNotFast$BLOCK_NAME)
  
  myTblNotFast[myTblNotFast$BLOCK_NAME %in% trialBlocks[c(1,3)], ]$blockPairs <- "S1"
  myTblNotFast[myTblNotFast$BLOCK_NAME %in% trialBlocks[c(2,4)], ]$blockPairs <- "S2"
  
  myTblNotFast <- group_by(myTblNotFast, SESSION_ID, blockPairs)
  
  allTblSDs <- merge(filter(myTblNotFast, BLOCK_NAME %in% trialBlocks) %>% summarise(A = sd(TRIAL_LATENCY)),
                     filter(myTblNotFast, BLOCK_NAME %in% trialBlocks & TRIAL_ERROR == 0) %>%
                       summarise(C = sd(TRIAL_LATENCY)),
                     by = c(sessionID, "blockPairs"))
  
  blockMeansTbl <- merge(reshape(allTblSDs, v.names = c("A", "C"), idvar = "SESSION_ID", timevar = "blockPairs", direction = "wide", sep = ""),
                         reshape(blockMeans, v.names = c("M", "N"), idvar = "SESSION_ID", timevar = "BLOCK_NAME", direction = "wide", sep = ""),
                         by = "SESSION_ID")
  
  # Step 7: Replace error latencies with block mean + 600ms
  # Already done in step above
  
  # Step 8: No transformation of latencies
  
  # Step 9: Average latencies for each of the four blocks
  # This was already done above
  
  # Step 10: Compute the two differences B6 - B3 and B7 - B4
  # Step 11: Divide each difference by associated pooled SD from step 6
  
  if(vStd == 1){
    
    tblResult <- 
      mutate(blockMeansTbl,
             DIFF1 = MBLOCK5 - MBLOCK2,
             DIFF2 = MBLOCK6 - MBLOCK3) %>%
      mutate(IAT1 = DIFF1/AS1,
             IAT2 = DIFF2/AS2) %>%
      mutate(IAT = (IAT1 + IAT2)/2)
    
  } else if(vStd == 2){
    
    tblResult <- 
      mutate(blockMeansTbl,
             DIFF1 = MBLOCK5 - MBLOCK2,
             DIFF2 = MBLOCK6 - MBLOCK3) %>%
      mutate(IAT1 = DIFF1/CS1,
             IAT2 = DIFF2/CS2) %>%
      mutate(IAT = (IAT1 + IAT2)/2)
    
  } else stop("Please enter a vStd value of 1 or 2.")
  
  ##########################
  # Create output dataframe
  ##########################
  
  myTbl <- group_by(myTbl, SESSION_ID, BLOCK_NAME)
  
  tblErrFast <- summarise(myTbl,
                          E = sum(TRIAL_ERROR)/length(TRIAL_ERROR),
                          F = sum(TRIAL_LATENCY < 300)/length(TRIAL_LATENCY))
  
  tblErrFastWide <- reshape(tblErrFast, v.names = c("E", "F"), idvar = "SESSION_ID", timevar = "BLOCK_NAME", direction = "wide", sep = "")
  
  tblExtras <- merge(tblErrFastWide, myFastTbl, by = sessionID, all = TRUE)
  
  myTbl <- group_by(myTbl, SESSION_ID)
  
  tblExcl <- summarise(myTbl, SUBEXCL = unique(SUBEXCL))
  
  tblExtras$SUBEXCL <- tblExcl$SUBEXCL
  
  tblTotal <- merge(tblResult, tblExtras, by = sessionID, all = TRUE)
  
  names(tblTotal)[names(tblTotal) == "SESSION_ID"] <- sessionID
  
  return(tblTotal)
  
}
