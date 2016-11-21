rankall <- function(varOutcome, varRank = "best") {
        ## Read outcome data
        masterData <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
        
        ## Check that outcome is valid
        validOutcome <- c("heart attack","heart failure","pneumonia") == varOutcome
        if(sum(validOutcome) < 1) {
                stop("invalid outcome")
        }
        if(varOutcome == "heart attack") { masterCol <- 11 }
        if(varOutcome == "heart failure") { masterCol <- 17 }
        if(varOutcome == "pneumonia") { masterCol <- 23 }
        
        ## For each state, find the hospital of the given rank at the given outcome
        listStates <- as.vector(unique(masterData[,7]))
        rankings <- data.frame(hospital=character(),state=character())
        for(i in 1:length(listStates)) {
                
                stateData <- subset(masterData, masterData[,7]==listStates[i])
                stateData <- stateData[ order(stateData[,masterCol], stateData[,2]), ]
                x <- is.na(stateData[,masterCol])
                stateData <- stateData[!x,]
                if(varRank == "best") { 
                        stateRank <- as.numeric(1)
                } else {
                        if(varRank == "worst") {
                                stateRank <- as.numeric(nrow(stateData))
                        } else {
                                stateRank <- as.numeric(varRank)
                        }
                }
                rankings <- rbind(rankings,stateData[stateRank,c(2,7)])
        }
        
        ## Return a data frame with the hospital names and the (abbreviated) state name
        rankings
}