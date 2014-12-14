pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    # If str_pad is to be used, the next line should be included prior to for()
    # library("stringr")

    # Creation of data frame to store the data from all monitors defined
    alldata <- data.frame()
    cclass <- c("Date", "numeric", "numeric", "integer")
    for(i in id) {
        # Determination of input file and path
        # Alternative to sprintf() the following functions can be used
        # filename <- str_pad(as.character(i), width = 3, side = "left", pad = "0")
        # filename <- formatC(i, width = 3, format = "d", flag = "0")
        filename <- sprintf("%03d%4s", i, ".csv")
        filepath <- paste(directory, filename, sep = "/")
        # Data import from file with ID i
        filedata <- read.csv(filepath, colClasses = cclass)
        
        # Append of data to previous monitors' data
        alldata <- rbind(alldata, filedata)
    }
    
    # Calculation of mean ignoring NA's
    mean(alldata[,pollutant], na.rm = TRUE)
}