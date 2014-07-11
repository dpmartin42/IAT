data("IATData", envir = .BaseNamespaceEnv)

# Get Ps who recieve Math-Male sorting task in first blocks

myDataCongFirst <- IATData[IATData$isCongruentFirst == 1, ]

myDScoreCongFirst <- cleanIAT(myData = myDataCongFirst,
                              blockName = "BLOCK_NAME_S",
                              trialBlocks = c("BLOCK2", "BLOCK3", "BLOCK5", "BLOCK6"),
                              sessionID = "SESSION_ID",
                              trialLatency = "TRIAL_LATENCY",
                              trialError = "TRIAL_ERROR",
                              vError = 1, vExtreme = 2, vStd = 1)

# Get Ps who recieve Math-Female sorting task in first blocks

myDataCongSec <- IATData[IATData$isCongruentFirst == 0, ]

myDScoreCongSec <- cleanIAT(myData = myDataCongSec,
                            blockName = "BLOCK_NAME_S",
                            trialBlocks = c("BLOCK2", "BLOCK3", "BLOCK5", "BLOCK6"),
                            sessionID = "SESSION_ID",
                            trialLatency = "TRIAL_LATENCY",
                            trialError = "TRIAL_ERROR",
                            vError = 1, vExtreme = 2, vStd = 1)

myDScore <- rbind(myDScoreCongFirst, myDScoreCongSec)

