\name{cleanIAT}
\alias{cleanIAT}
\title{Clean IAT data using the updated D-Scoring algorithm}
\usage{
  cleanIAT(myData, blockName, trialBlocks, sessionID,
    trialLatency, trialError, vError, vExtreme, vStd)
}
\arguments{
  \item{myData}{The raw dataframe to be used}

  \item{blockName}{A string of the variable name for the
  blocks}

  \item{trialBlocks}{A vector of the four essential blocks
  in the seven-block IAT (i.e., B3, B4, B6, and B7).}

  \item{sessionID}{A string of the variable name
  identifying each unique participant.}

  \item{trialLatency}{A string of the variable name for the
  latency of each trial.}

  \item{trialError}{A string of the variable name
  identifying whether a trial was an error or not, where 1
  indicates an error.}

  \item{vError}{If 1 (current standard), then means are
  calculated for the entire set of latencies. If 2, error
  latencies will be replaced by the block mean + 600ms}

  \item{vExtreme}{If 1, then no extreme value treatment. If
  2 (current standard), delete trial latencies < 400ms}

  \item{vStd}{If 1 (current standard), block SD is
  performed including error trials (corrected or not). If
  2, block SD is performed on correct responses only}
}
\value{
  Outputs a dataframe that must be saved to an object. The
  variable IAT is the calculated D-Score for each
  individual. SUBEXCL notes any exclusion criteria, with 0
  being inclusion data, 1 for exclusion due to fast
  response, and 2 for exclusion due to missing blocks. C
  indicates standard deviation for combined blocks (correct
  trial only), while A indicates standard deviations for
  combined blocks (all trials). M (mean), E (percent
  error), N (number of trials used), and F (percent fast
  responses), are reported for each block included in the
  original dataframe.
}
\description{
  This macro will transform a dataframe with trial
  latencies (stored as one line per trial) for a standard
  format IAT (7 blocks) into a one line summary per subject
  of the IAT effect using GNB's new scoring algorithm. The
  goal of this functionis to prepare IAT data for
  subsequent analysis. However, this does not relieve the
  researcher from making conceptual decisions about how
  best to analyze IAT data.  There are decisions to make
  about how the function is applied, and the function does
  not remove participants.  All subject exclusions must be
  made deliberately by the researcher.
}
\examples{
data("IATData", envir = .BaseNamespaceEnv)

# Get Ps who recieve Math-Male 
# sorting task in first blocks
myDataCongFirst <- IATData[IATData$isCongruentFirst == 1, ]

myDScoreCongFirst <- cleanIAT(myData = myDataCongFirst,
                              blockName = "BLOCK_NAME_S",
                              trialBlocks = paste0("BLOCK", c(2, 3, 5, 6)),
                              sessionID = "SESSION_ID",
                              trialLatency = "TRIAL_LATENCY",
                              trialError = "TRIAL_ERROR",
                              vError = 1, vExtreme = 2, vStd = 1)

# Get Ps who recieve Math-Female 
# sorting task in first blocks
myDataCongSec <- IATData[IATData$isCongruentFirst == 0, ]

myDScoreCongSec <- cleanIAT(myData = myDataCongSec,
                            blockName = "BLOCK_NAME_S",
                            trialBlocks = paste0("BLOCK", c(2, 3, 5, 6)),
                            sessionID = "SESSION_ID",
                            trialLatency = "TRIAL_LATENCY",
                            trialError = "TRIAL_ERROR",
                            vError = 1, vExtreme = 2, vStd = 1)

myDScore <- rbind(myDScoreCongFirst, myDScoreCongSec)

# Print D-Scores for all Ps
myDScore$IAT
}
\references{
  \href{http://faculty.washington.edu/agg/pdf/GB&N.JPSP.2003.pdf}{Understanding
  and Using the Implicit Association Test: I. An Improved
  Scoring Algorithm}
}

