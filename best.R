best <- function(varState, varOutcome) {
        ## Read outcome data
        masterData <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that state and outcome are valid
        data(state)
        validState <- state.abb == varState
        if(sum(validState) < 1) {
                stop("invalid state")
        }
        
        validOutcome <- c("heart attack","heart failure","pneumonia") == varOutcome
        if(sum(validOutcome) < 1) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        if(varOutcome == "heart attack") { masterCol <- 11 }
        if(varOutcome == "heart failure") { masterCol <- 17 }
        if(varOutcome == "pneumonia") { masterCol <- 23 }
        stateData <- masterData[ which(masterData[,7] == varState), ]
        x <- stateData[,masterCol] == "Not Available"
        remData <- stateData[!x, ]
        remData[,masterCol] <- as.numeric(remData[,masterCol])
        sortedData <- remData[ order(remData[,masterCol], remData[,2]), ]
        print(as.vector(sortedData[1,2]))
        
}