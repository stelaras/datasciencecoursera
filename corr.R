corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations

    # If str_pad is to be used, the next line should be included prior to for()
    # library("stringr")
    
    cclass <- c("Date", "numeric", "numeric", "integer")
    # Sourcing of function complete()
    source("complete.R")
    cc <- complete(directory)  # Complete cases data frame
    # Subset of complete cases over threshold
    overThreshold <- cc[cc$nobs > threshold,]
    n <- nrow(overThreshold)
    # Initialization of correlations vector
    correlations <- numeric(n)
    if(n > 0) {
        for(i in 1:n) {
            # Determination of input file and path
            # Alternative to sprintf() the following functions can be used
            # filename <- str_pad(overThreshold[i,1], width = 3, side = "left", pad = "0")
            # filename <- formatC(overThreshold[i,1], width = 3, format = "d", flag = "0")
            filename <- sprintf("%03d%4s", overThreshold[i,1], ".csv")
            filepath <- paste(directory, filename, sep = "/")
            # Data import from file with ID overThreshold[i,1]
            filedata <- read.csv(filepath, colClasses = cclass)
            sVector <- filedata$sulfate
            nVector <- filedata$nitrate
            # Calculation of correlation for i monitor over threshold
            correlations[i] <- cor(sVector, nVector, use = "complete.obs")
        }
    }
    return(correlations)
}