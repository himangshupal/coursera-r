corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        completeData <- complete(directory)
        correlations <- computeSulphateAndNitrateCorrelations(directory, completeData, threshold)
}

computeSulphateAndNitrateCorrelations <- function(directory, completeData, threshold) {
	corrs <- numeric(length = 0)
	corrs <- apply(completeData, 1, computeCorrelations, threshold, directory)
	corrs<-as.numeric(corrs)
	corrs
}

computeCorrelations <- function(x, threshold, directory) {
	cr <- numeric(length = 0)
	print(paste("x[1] = ", x[1], sep=""))
	print(paste("x[2] = ", x[2], sep=""))
	if(as.numeric(x[2]) > threshold) {
		df <- getCompleteDataFrame(as.numeric(x[1]), directory)
		print(paste("cor= ",cor(df$nitrate, df$sulfate), sep=""))
		cr<-c(cr, cor(df$nitrate, df$sulfate))
	}
	cr
}