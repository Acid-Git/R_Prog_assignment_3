best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  
  ##select columns and set colnames
  data <- data.frame(data[, 2], data[, 7], data[, 11], data[, 17], data[, 23])
  colnames(data) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia" )
  
  ## Check that state and outcome are valid
  if(!(state %in% data$State)) {stop ("invalid state")} 
  if(!(outcome %in% colnames(data))) {stop ("invalid outcome")}

  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## get only hospital and death rate of given outcome in given state
  result <- subset(data, data$State == state , select = c("Hospital", outcome))
  
  ## get name of hospital with minimum death rate 
  result <- subset(result, result[, 2] == min(result[, 2], na.rm=TRUE), select = c("Hospital"))
  
  ##sort hospitals alphabetically and print first hospital
  sort(result$Hospital)[1]
}