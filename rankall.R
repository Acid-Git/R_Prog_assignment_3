rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  
  ##select columns and set colnames
  data <- data.frame(data[, 2], data[, 7], data[, 11], data[, 17], data[, 23])
  colnames(data) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia" )
  
  ## For each state, find the hospital of the given rank
  result <- data.frame();
  
  for (state in split(data, data$State)) {
    ##select correct outcome
    tmp <- subset(state, select = c("Hospital", "State", outcome))
    ##sort by outcome and afterwards by hospital name
    tmp <- tmp[ order(tmp[, 3], tmp[, 1], na.last = NA), ]
    #print(tail(tmp))
    ##add hospital name and state to result dataframe
    if (is.numeric(num)) {
      result <- rbind(result, c(tmp[num, 1], tmp[1, 2]))
    }
    else if (num == "best") {
      result <- rbind(result, c(tmp[1, 1], tmp[1, 2]))
    }
    else if (num =="worst") {
      result <- rbind(result, c(tmp[nrow(tmp),1], tmp[1, 2]))
    }
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  colnames(result) <- c("hospital", "state")
  rownames(result) <- result$state

  return(result)
  
}