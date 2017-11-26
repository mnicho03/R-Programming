best <- function (state, outcome) {
  
  #ensure valid outcome type
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
      stop("invalid outcome")
  }
  
  #set column focus based on outcome
  index <- if(outcome == "heart attack")
    11
  else if (outcome == "heart failure")
    17
  else 
    23
  
  #read and get complete data based on index
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- na.omit(data)
  data[,index] <- suppressWarnings(as.numeric(data[,index]))
  
  #ensure valid state entry
  allstates <- unique(data$State)
  if (!state %in% allstates) {
    stop("invalid state")
  }
  
  #get data per state
  relevantData <- subset(data, State==state)

  #find best rate
  sortedData <- relevantData[order(relevantData[,index], na.last = TRUE), 2]
  sortedData <- na.omit(sortedData)
  bestHospital <- sortedData[1]
  
  bestHospital
}