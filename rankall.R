## This function takes an outcome and a hospital ranking,
## and returns a data frame containing the hospital in each state
## that has the specified ranking and the respective state abbreviation.
## The function accepts an outcome among "heart attack", "heart failure",
## and "pneumonia"; reads the outcome-of-care-measures.csv file
## and returns a value for each state. Acceptable values for ranking
## are positive integers, as well as the values "best" and "worst".

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", 
                     colClasses = "character")
    ## Check that outcome is valid
    ## Valid outcome names
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!(outcome %in% validOutcomes)) {
        stop("invalid outcome")
    }
    if(num == "best") {
        n <- as.numeric(1)
    } else {
        n <- suppressWarnings(as.numeric(num))
    }
    ## Outcome selection
    out <- c(11, 17, 23)[match(outcome, validOutcomes)]
    ## Valid states
    validStates <- sort(unique(data$State))
    hospitalVector <- character(length = length(validStates))
    ## For each state, find the hospital of the given rank
    for(state in validStates) {
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
        if(num == "worst") {
            n <- numberOfHospitals
        }
        ## Return hospital name in that state with the given rank (n)
        ## 30-day death rate
        if(n > numberOfHospitals) {
            hospitalVector[match(state, validStates)] <- NA
        } else {
            hospitalVector[match(state, validStates)] <- stateMatrix[ranking[n], 2]
        }
    }
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    output <- data.frame(hospitalVector, validStates)
    names(output) <- c("hospital", "state")
    return(output)
}