rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv" ,colClasses = "character")
  suppressWarnings(data[, 11] <- as.numeric(data[, 11]))
  suppressWarnings(data[, 17] <- as.numeric(data[, 17]))
  suppressWarnings(data[, 23] <- as.numeric(data[, 23]))
  
  # validate outcome
  outcomes = c("heart attack" , "heart failure" , "pneumonia")
  if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
  
  # name the columns as outcomes
  data <- data[c(2,7,11,17,23)]
  names(data)[1] = "hospital"
  names(data)[2] ="state"
  names(data)[3]="heart attack"
  names(data)[4]="heart failure"
  names(data)[5]="pneumonia"
  
  # validate state
  
  states <- unique(data[,2])
  if( state %in% states == FALSE) stop ("invalid state")
  
  ## Validate the num value
  if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
  
  # order hospital 
  data <- data[complete.cases(data),]
  data <- data[data$state==state,]
  data <- data[order(data$hospital, decreasing = FALSE), ]
  data <- data[order(data[outcome], decreasing = FALSE), ]
  
  vals      <- data[,outcome]   
  ## Return hospital name in that state with the given rank
  if (num == "best")
  {rowNum <- which.min(vals)}
  if (num == "worst")
  {rowNum <- which.max(vals)}
  else {rowNum <- num}
  ## Return hospital name in that state with the given rank
  data[rowNum, ]$hospital
}
