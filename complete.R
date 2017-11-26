# Write a function that reads a directory full of files and reports the number of 
# completely observed cases in each data file. The function should return a data 
# frame where the first column is the name of the file and the second column is the 
# number of complete cases. 

complete <- function(directory, id = 1:332) {
  
  nobs = numeric()
  
  #set number of rows to calc
  id_len <- length(id)
  complete <- rep(0, id_len)
  base <- 1
  
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
    
    #read csv data
    csv = read.csv(paste(directory, ".csv", sep = ""))
    
    #create list of full obs
    complete[base] <- sum(complete.cases(csv))
    
    base <- base + 1
  
  }
    
    #create data frame 
    df <- data.frame(id = id, nobs = complete)
    
    return(df)
}