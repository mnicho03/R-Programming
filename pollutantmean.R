# Write a function named 'pollutantmean' that calculates the mean of a pollutant 
# (sulfate or nitrate) across a specified list of monitors. The function 
# 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a 
# vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter 
# data from the directory specified in the 'directory' argument and returns the mean 
# of the pollutant across all of the monitors, ignoring any missing values coded as 
# NA. A prototype of the function is as follows

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  #ensure valid pollutant input
  if (!pollutant %in% c("sulfate", "nitrate")) stop("invalid pollutant")
  
  data = numeric()
  
#read in data based on id's
  for (i in id) {
    
    #insert leading 0's
    if (i < 10) {
      directory <- paste("00",i, sep = "")
    } else if (i < 100) {
      directory <- paste("0",i, sep = "")
    }  else {
      directory
    }
    
    readit = read.csv(paste(directory, ".csv", sep = ""))
    
    data = c(data, readit[[pollutant]])
  }
  
  #get mean of the data (remove NA)
  
  return(mean(data, na.rm = TRUE))
  
 }
   
                          