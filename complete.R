complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases

    # If str_pad is to be used, the next line should be included prior to for()
    # library("stringr")
    
    cclass <- c("Date", "numeric", "numeric", "integer")
    n <- length(id)  # Number of monitors to be used
    # Initialization of "id" and "nobs" vectors
    idVector <- integer(n)
    nobsVector <- integer(n)
    for(i in seq_along(id)) {
        # Determination of input file and path
        # Alternative to sprintf() the following functions can be used
        # filename <- str_pad(as.character(id[i]), width = 3, side = "left", pad = "0")
        # filename <- formatC(id[i], width = 3, format = "d", flag = "0")
        filename <- sprintf("%03d%4s", id[i], ".csv")
        filepath <- paste(directory, filename, sep = "/")
        # Data import from file with ID[i]
        filedata <- read.csv(filepath, colClasses = cclass)
        # Assignment of values to respective cases of vectors
        idVector[i] <- id[i]
        nobsVector[i] <- nrow(filedata[complete.cases(filedata),])
    }
    # Merging of vectors to data frame
    completeData <- data.frame(idVector, nobsVector, stringsAsFactors = FALSE)
    colnames(completeData) <- c("id", "nobs")
    return(completeData)
}