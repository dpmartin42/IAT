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
