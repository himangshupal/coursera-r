complete <- function(directory, ids = 1:332) {
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

        id <- c(ids)

        nobs <- c()
        for(index in ids) {
                nobs <- c(nobs, getNumberOfCompletedCases(index, directory))
        }
        resultDataFrame= data.frame(id, nobs )
        resultDataFrame

}

getFileName <- function(id) {
        numId <- as.numeric(id)
        if(numId < 10) {
                numId <- paste('00', numId, sep="")
        }else if(numId > 9  && numId < 100) {
                numId <- paste('0', numId, sep="")
        } 
        paste(numId, "csv", sep=".")
}

getNumberOfCompletedCases <- function(id, directory) {
        fileName <- getFileName(id)
        csvData <- read.csv(paste(directory, fileName, sep="/"))
        completeCSVData <- csvData[complete.cases(csvData),]
        nrow(completeCSVData)
}

getCompleteDataFrame <- function(id, directory) {
        fileName <- getFileName(id)
        csvData <- read.csv(paste(directory, fileName, sep="/"))
        row.has.na <- apply(csvData, 1, function(x) {any(is.na(x))})
        csvData[!row.has.na,]
}

getNumberOfCompletedCasesUsingApply <- function(id, directory) {
        nrow(getCompleteDataFrame(id, directory))
}