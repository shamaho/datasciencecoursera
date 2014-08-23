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
  
  
  ## Build up filename and read file to extract class data
  currwd <- getwd()

  # Define a blank data frame to contain data from all requested monitors
  nobsFrame <- data.frame (
          id = integer(0), 
          nobs = integer(0)
  )
  
  ## Loop through the requested monitors in id param. and read data
  for( i in id ) {
    
    ## build up the filename to be read
    filename <- as.character(paste(currwd, "/", directory, "/", formatC(i, width=3, flag="0"), ".csv", sep=""))
    #print(filename)
    
    ## read the file contants into data frame
    MonitorData <- read.table(filename, header = TRUE, sep=",")
    
    # Find the number of complete cases for the file
    complete <- complete.cases( MonitorData )    
    numComplete <- nrow( MonitorData[ complete, ] )
    
    ## append to the global frame containing data from all requested monitors
    nobsFrame <- rbind( nobsFrame, c(i, numComplete))
  }

  names(nobsFrame) <- c("id", "nobs")
  nobsFrame
}