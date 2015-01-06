## This function takes the 2-character abbreviated name of a state
## and an outcome name among "heart attack", "heart failure" and
## "pneumonia", and returns a character vector with the name of
## the hospital that has the lowest 30-day mortality rate on the 
## particular outcome in the specified state.

best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", 
                     colClasses = "character")
    ## Valid states
    validStates <- unique(data$State)
    ## Valid outcome names
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    ## Check that state and outcome are valid
    if(!is.element(state, validStates)) {
        stop("invalid state")
    }
    if(!(outcome %in% validOutcomes)) {
        stop("invalid outcome")
    }
    ## Outcome selection
    out <- c(11, 17, 23)[match(outcome, validOutcomes)]
    ## Matrix with all data concerning the specific state
    stateMatrix <- data[data$State == state, ]
    stateMatrix[, out] <- suppressWarnings(as.numeric(stateMatrix[, out]))
    ## Position of lowest rate among the state's hospitals
    p <- which.min(stateMatrix[, out])
    ## Return hospital name in that state with lowest 30-day death rate
    return(stateMatrix[p, 2])
}