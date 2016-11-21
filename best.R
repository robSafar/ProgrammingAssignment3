best <- function(varState, varOutcome) {
        ## Read outcome data
        masterData <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
        
        ## Check that state and outcome are valid
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
        
        ## Return hospital name in that state with lowest 30-day death rate
        stateData <- masterData[ which(masterData[,7] == varState), ]
        sortedData <- stateData[ order(stateData[,masterCol], stateData[,2]), ]
        print(as.vector(sortedData[1,2]))
        
}