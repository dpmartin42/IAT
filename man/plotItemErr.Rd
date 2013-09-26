\name{plotItemErr}
\alias{plotItemErr}
\title{Plot proportion of errors per item in the IAT}
\usage{
  plotItemErr(myData, blockName, trialBlocks, sessionID,
    itemName, trialLatency, trialError)
}
\arguments{
  \item{myData}{The raw dataframe to be used}

  \item{blockName}{A string of the variable name for the
  blocks}

  \item{trialBlocks}{A vector of the four essential blocks
  in the seven-block IAT (i.e., B3, B4, B6, and B7).}

  \item{sessionID}{A string of the variable name
  identifying each unique participant.}

  \item{itemName}{A string of the variable identifying the
  items}

  \item{trialLatency}{A string of the variable name for the
  latency of each trial.}

  \item{trialError}{A string of the variable name
  identifying whether a trial was an error or not (1 =
  error)}
}
\description{
  This function uses ggplot2 to plot proportion of errors
  in the IAT to see if any items yield high error rates.
  The data is automatically cleaned (i.e., trials with RT >
  10000 or < 180 are deleted) to avoid over/underinflation
  of mean estimates and only include trials from essential
  blocks.
}

