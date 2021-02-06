rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  
  ##select columns and set colnames
  data <- data.frame(data[, 2], data[, 7], data[, 11], data[, 17], data[, 23])
  colnames(data) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia" )
  
  ## Check that state and outcome are valid
  if(!(state %in% data$State)) {stop ("invalid state")} 
  if(!(outcome %in% colnames(data))) {stop ("invalid outcome")}
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  result <- subset(data, data$State == state , select = c("Hospital", outcome))
  
  result <- result[ order(result[, 2], result[, 1], na.last=NA), ]
  
  if (is.numeric(num)) {
    result[num, 1]  
  }
  else if (num == "best") {
    result[1, 1]
  }
  else if (num == "worst") {
    result[nrow(result), 1]
  }
}
 