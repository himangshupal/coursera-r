getmonitor <- function(id, directory, summarize = FALSE) {
        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSE
        
        ## Your code here
        fileName <- 
        csvData <- read.csv(paste(directory, getFileName(id), sep="/"))
        if(summarize) {
        	print(summary(csvData))
        }
      	csvData
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