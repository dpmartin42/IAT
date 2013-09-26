#' Sample Gender Stereotype Implicit Association Test data
#'
#' @name IATData
#' @docType data
#' @description A dataframe containing data from a Gender Stereotype Implicit Association Test. Data was taken from college students in a 
#' differential equations classroom taught by a female professor.
#' @format A dataframe with 11792 observations of 16 variables (88 students in total)
#' 
#' \itemize{
#'   \item BLOCK_NAME_S: string of blocknames
#'   \item BLOCK_PAIRING_DEFINITION_S: string of block pairings
#'   \item TRIAL_NAME_S: word/picture used in sorting trial
#'   \item SESSION_ID: ID of participant
#'   \item TRIAL_NUMBER: number of trial within block
#'   \item TRIAL_ERROR: indicates whether trial was an error (1 = YES)
#'   \item TRIAL_LATENCY: reaction time for trial
#'   \item isCongruentFirst: indicates if stereotype congruent blocks came first
#' }
#' 
#' 
#' @author Dan Martin {dm4zz@@virginia.edu}
#' @references \url{http://projectimplicit.net/fpi/researchers.html}
#' @keywords IAT Implicit Association Test
NULL

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
#' @example R/cleanIATExample.R
#' @import data.table
#' @export

cleanIAT <- function(myData, blockName, trialBlocks, sessionID, trialLatency, trialError, vError, vExtreme, vStd){
  
  # To appease global variable check
  FASTM <- NULL; SUBEXCL <- NULL; myBlockMean <- NULL; DIFF1 <- NULL; DIFF2 <- NULL;
  IAT <- NULL; IAT1 <- NULL; IAT2 <- NULL; AS1 <- NULL; AS2 <- NULL; CS1 <- NULL; CS2 <- NULL
  
  myDataTable <- data.table(myData)
  
  myDataTable$SUBEXCL <- 0
  
  # Step 1: Include data from B3, B4, B6, B7
  # Step 1 has been removed so all data is at least partially analyzed
  
  # Step 2a: Eliminate trial latencies > 10,000ms and < 0ms
  
  myDataTableNoLong <- myDataTable[get(trialLatency) < 10000 & get(trialLatency) >= 0, ]
  
  # Step 2b: Mark subjects for whom more than 10% of trials have latencies < 300ms with a 1
  
  myFastTable <- myDataTable[get(blockName) %in% trialBlocks,
                             list(FASTM = sum(get(trialLatency) < 300)/length(get(trialLatency))),
                             by = list(ID = get(sessionID))]
  
  isTooFastTable <- myFastTable[FASTM > 0.10,]$ID
  setnames(myFastTable, "ID", sessionID)
  
  suppressWarnings(invisible(if(length(isTooFastTable) > 0){myDataTable[get(sessionID) %in% isTooFastTable, SUBEXCL := as.integer(1)]}))
  
  # Step 2c: Mark subjects with any missing blocks with a 2
  
  numBlocksTable <- myDataTable[, sum(unique(get(blockName)) %in% trialBlocks), by = get(sessionID)]
  
  notCompleteTable <- numBlocksTable[numBlocksTable$V1 < 4, get]
  
  suppressWarnings(invisible(if(length(notCompleteTable) > 0){myDataTable[get(sessionID) %in% notCompleteTable, SUBEXCL := as.integer(2)]}))
  
  # Step 3: Use all trials (in the conventional algorithm) the first two trials of each
  # block would be dropped here
  
  # Step 4: No extreme value treatment <or> delete trial with latencies < 400ms
  
  if(vExtreme == 1){
    
    myDataTableNotFast <- myDataTableNoLong
    
  } else if(vExtreme == 2){
    
    myDataTableNotFast <- myDataTableNoLong[get(trialLatency) >= 400, ]
    
  } else stop("Please pick a value of 1 or 2 for vExtreme.")
  
  # Step 5: Compute mean of correct latencies for each block
  # If vError = 1 then means and SDs will be calculated for the entire set of latencies
  # IF vError = 2 then the algorithm will replace error trial latencies with blockmean + 600
  # (where blockmean is mean of correct responses only)
  
  # Step 5: Compute mean of correct latencies for each block
  # If vError = 1 then means and SDs will be calculated for the entire set of latencies
  # IF vError = 2 then the algorithm will replace error trial latencies with blockmean + 600
  # (where blockmean is mean of correct responses only)
  
  if(vError == 1){
    
    blockMeansTable <- myDataTableNotFast[, list(M = mean(get(trialLatency)), N = length(get(trialLatency))), by = list(ID = get(sessionID), Block = get(blockName))]
    
    setnames(blockMeansTable, "Block", blockName)
    
  } else if (vError == 2){
    
    myMeanReplaceTable <- myDataTableNotFast[get(trialError) == 0, list(myBlockMean = mean(get(trialLatency)) + 600), by = list(ID = get(sessionID), block = get(blockName))]
    
    setnames(myMeanReplaceTable, c("ID", "block"), c(sessionID, blockName))
    
    myMergeTable <- merge(myDataTableNotFast, myMeanReplaceTable, by = c(sessionID, blockName), all = TRUE)
    
    myDataTableNotFast$tmpLatency <- myDataTableNotFast[, get(trialLatency)]
    
    suppressWarnings(myDataTableNotFast[get(trialError) == 1, ]$tmpLatency <- myMergeTable[get(trialError) == 1, myBlockMean])
    
    setnames(myDataTableNotFast, c(trialLatency, "tmpLatency"), c("oldLatency", trialLatency))
    
    blockMeansTable <- myDataTableNotFast[, list(M = mean(get(trialLatency)), N = length(get(trialLatency))), by = list(ID = get(sessionID), Block = get(blockName))]
    
    setnames(blockMeansTable, "Block", blockName)
    
  } else stop("Please pick a value of 1 or 2 for vError.")
  
  # Step 6: Compute pooled SD for B3 & B6, and separately for B4 & B7.
  # If vStd is 1, the block SD is performed including error trials (corrected or not)
  # If vStd is 2, the block SD is performed on correct responses only
  
  myDataTableNotFast$blockPairs <- as.character(myDataTableNotFast[, get(blockName)])
  
  myDataTableNotFast[get(blockName) %in% trialBlocks[c(1,3)], ]$blockPairs <- "S1"
  myDataTableNotFast[get(blockName) %in% trialBlocks[c(2,4)], ]$blockPairs <- "S2"
  
  bloPaString <- "blockPairs"
  
  allTableSDs <- merge(myDataTableNotFast[get(blockName) %in% trialBlocks, list(A = sd(get(trialLatency))), by = list(ID = get(sessionID), blockPairs = get(bloPaString))],
                       myDataTableNotFast[get(blockName) %in% trialBlocks & get(trialError) == 0, list(C = sd(get(trialLatency))), by = list(ID = get(sessionID), blockPairs = get(bloPaString))],
                       by = c("ID", "blockPairs"))
  
  blockMeansTableWide <- merge(reshape(allTableSDs, v.names = c("A", "C"), idvar = "ID", timevar = "blockPairs", direction = "wide", sep = ""),
                               reshape(blockMeansTable, v.names = c("M", "N"), idvar = "ID", timevar = blockName, direction = "wide", sep = ""),
                               by = "ID")
  
  # Step 7: Replace error latencies with block mean + 600ms
  # Already done in step above
  
  # Step 8: No transformation of latencies
  
  # Step 9: Average latencies for each of the four blocks
  # This was already done above
  
  # Step 10: Compute the two differences B6 - B3 and B7 - B4
  # Step 11: Divide each difference by associated pooled SD from step 6
  
  invisible(
    if(vStd == 1){
      
      blockMeansTableWide[, DIFF1 := get(paste0("M", trialBlocks[3])) - get(paste0("M", trialBlocks[1]))]
      blockMeansTableWide[, DIFF2 := get(paste0("M", trialBlocks[4])) - get(paste0("M", trialBlocks[2]))]
      blockMeansTableWide[, IAT1 := DIFF1/AS1]
      blockMeansTableWide[, IAT2 := DIFF2/AS2]
      blockMeansTableWide[, IAT := (IAT1 + IAT2)/2]
      
    } else if(vStd == 2){
      
      blockMeansTableWide[, DIFF1 := get(paste0("M", trialBlocks[3])) - get(paste0("M", trialBlocks[1]))]
      blockMeansTableWide[, DIFF2 := get(paste0("M", trialBlocks[4])) - get(paste0("M", trialBlocks[2]))]
      blockMeansTableWide[, IAT1 := DIFF1/CS1]
      blockMeansTableWide[, IAT2 := DIFF2/CS2]
      blockMeansTableWide[, IAT := (IAT1 + IAT2)/2]
      
    } else stop("Please enter a vStd value of 1 or 2.")
  )
  
  setnames(blockMeansTableWide, "ID", sessionID)
  
  ##########################
  # Create output dataframe
  ##########################
  
  myDataTableErrFast <- myDataTable[, list(E = sum(get(trialError)/length(get(trialError))),
                                           F = sum(get(trialLatency) < 300)/length(get(trialLatency))),
                                    by = list(ID = get(sessionID), block = get(blockName))]
  
  myDataTableErrFastWide <- reshape(myDataTableErrFast, v.names = c("E", "F"), idvar = "ID", timevar = "block", direction = "wide", sep = "")
  setnames(myDataTableErrFastWide, "ID", sessionID)
  
  myDataTableExtras <- merge(myDataTableErrFastWide, myFastTable, by = sessionID, all = TRUE)
  
  myDataTableExcl <- myDataTable[,list(SUBEXCL = unique(SUBEXCL)), by = list(ID = get(sessionID))]
  
  myDataTableExtras$SUBEXCL <- myDataTableExcl[,SUBEXCL]
  
  myDataTableTotal <- merge(blockMeansTableWide, myDataTableExtras, by = sessionID, all = TRUE)
  
  invisible(as.data.frame(myDataTableTotal))
  
}
