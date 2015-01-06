## This function takes a 2-character abbreviated state name,
## an outcome among "heart attack", "heart failure", and "pneumonia",
## and a ranking for a hospital in that state for that outcome.
## The function reads the outcome-of-care-measures.csv file
## and returns the name of the hospital that has the specified ranking
## in that state. Acceptable values for ranking are positive integers,
## as well as the values "best" and "worst".

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", 
                     colClasses = "character")
    ## Check that state and outcome are valid
    ## Valid states
    validStates <- unique(data$State)
    ## Valid outcome names
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!is.element(state, validStates)) {
        stop("invalid state")
    }
    if(!(outcome %in% validOutcomes)) {
        stop("invalid outcome")
    }
    ## Outcome selection
    out <- c(11, 17, 23)[match(outcome, validOutcomes)]
    ## Matrix with all data concerning the specified state hospitals
    ## with valid data on the particular outcome
    stateMatrix <- data[data$State == state, ]
    ## The outcome columns MUST be coerced to numeric
    ## before excluding NA's
    stateMatrix[, out] <- suppressWarnings(as.numeric(stateMatrix[, out]))
    numberOfHospitals <- sum(!is.na(stateMatrix[, out]))
    ## Ranking of specified state's hospitals according to outcome
    ranking <- order(stateMatrix[, out], stateMatrix[, 2])
    ## The value of argument 'num' is checked and altered if necessary
    if(num == "best") {
        num <- as.integer(1)
    } else if(num == "worst") {
        num <- as.integer(numberOfHospitals)
    } else if(as.integer(num) > numberOfHospitals) {
        return(NA)
    } else {
        num <- as.integer(num)
    }
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    return(stateMatrix[ranking[num], 2])
}