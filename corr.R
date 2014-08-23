corr <- function(directory, threshold = 0) {

#   takes a directory of data files and a threshold for complete cases and calculates the 
#   correlation between sulfate and nitrate for monitor locations where the number of 
#   completely observed cases (on all variables) is greater than the threshold. The function 
#   should return a vector of correlations for the monitors that meet the threshold 
#   requirement. If no monitors meet the threshold requirement, then the function should 
#   return a numeric vector of length 0

## Function parameters are:
##  - 'directory' is a character vector of length 1 indicating the location of the
## CSV files  
##  - 'threshold' is a numeric vector of length 1 indicating the number of
## completely observed observations (on all variables) required to compute the
## correlation between nitrate and sulfate; the default is 0

  # get cwd to Build up filename and read monitor files
  currwd <- getwd()

  # declare the return vector
  resultvect <- vector( "numeric", length = 0)

  
  # Get count of complete cases per monitor file in the directory
  CompleteMonitorData <- complete( directory )
  
  # identify which rows have complete cases obeying threshold value
  threshold_check <- CompleteMonitorData["nobs"] > threshold

  # get data frame filtering for Monitor IDs obeying to threshold value
  validMonitorList <- CompleteMonitorData[ threshold_check, ]

  if ( nrow(validMonitorList) > 0) {
    
    # Loop through each valid monitor (obeying threshold for compelte cases)
    for (i in validMonitorList[,"id"] ) {
  
      # build up the filename to be read
      filename <- as.character(paste(currwd, "/", directory, "/", formatC(i, width=3, flag="0"), ".csv", sep=""))      
      #print(filename)
      
      # read the file contants into data frame
      MonitorData <- read.table(filename, header = TRUE, sep=",")
      
      # Find/Get the number of complete cases for the file
      complete <- complete.cases( MonitorData )
      datacomplete <- MonitorData[ complete, ]
      
      # Compute correlation
      corvalue <- cor( datacomplete[ , "sulfate"], datacomplete[ , "nitrate"] )
    
      # add correlation to vector to be returned
      resultvect <- c( resultvect, corvalue )
    }
  }
  
  # Return the numeric vector of correlations
  resultvect
}