pollutantmean <- function(directory, pollutant, id = 1:332) {

  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  
  ## Build up filename and read file to extract class data
  currwd <- getwd()

  ## This part ended up not being used after all
  initfile  <- as.character(paste(currwd, "/", directory, "/", formatC( id[1], width=3, flag="0"), ".csv", sep =""))
  initial <- read.table( initfile, nrows = 50, skip=5, quote = "\"" )
  classdesc <- sapply( initial, class )

  # Define a blank data frame to contain data from all requested monitors
  AllMonitorData <- data.frame(
                               Date = character(0), 
                               sulfate = integer(0),
                               nitrate = integer(0),
                               ID = integer(0)
                               )
  
  ## Loop through the requested monitors in id param. and read data
  for( i in id ) {

    ## build up the filename to be read
    filename <- as.character(paste(currwd, "/", directory, "/", formatC(i, width=3, flag="0"), ".csv", sep=""))
    #print(filename)
    
    ## read the file contants into data frame
    MonitorData <- read.table(filename, header = TRUE, sep=",")

    ## add to the Global frame
    AllMonitorData <- rbind( AllMonitorData, MonitorData )
  }
  
  ## Get the NA values for selected pollutant
  badMonitor <- is.na( AllMonitorData[ pollutant ] )
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  mean( AllMonitorData[ !badMonitor, ][ , pollutant ] )    
}
