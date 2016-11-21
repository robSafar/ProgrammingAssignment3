rankhospital <- function(varState, varOutcome, varRank="best") {
        ## Read outcome data
        masterData <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that outcome is valid
        listStates <- unique(masterData[,7])
        validState <- listStates == varState
        if(sum(validState) < 1) {
                stop("invalid state")
        }
        
        validOutcome <- c("heart attack","heart failure","pneumonia") == varOutcome
        if(sum(validOutcome) < 1) {
                stop("invalid outcome")
        }
        if(varOutcome == "heart attack") { masterCol <- 11 }
        if(varOutcome == "heart failure") { masterCol <- 17 }
        if(varOutcome == "pneumonia") { masterCol <- 23 }
        
        ## Return hospital name in that state with the given rank
        stateData <- masterData[ which(masterData[,7] == varState), ]
        x <- stateData[,masterCol] == "Not Available"
        stateData <- stateData[!x, ]
        stateData[,masterCol] <- as.numeric(stateData[,masterCol])
        sortedData <- stateData[ order(stateData[,masterCol], stateData[,2]), ]
        if(varRank == "best") {
                stateRank <- as.numeric("1")
        } else {
                if(varRank == "worst") {
                        stateRank <- as.numeric(nrow(sortedData))
                } else {
                        stateRank <- as.numeric(varRank)
                }
        }
        print(as.vector(sortedData[stateRank,2]))
}