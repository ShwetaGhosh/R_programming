best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv" ,colClasses = "character")
  suppressWarnings(data[, 11] <- as.numeric(data[, 11]))
  suppressWarnings(data[, 17] <- as.numeric(data[, 17]))
  suppressWarnings(data[, 23] <- as.numeric(data[, 23]))

                   # validate outcome
  outcomes = c("heart attack" , "heart failure" , "pneumonia")
  if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
  
  data <- data[c(2,7,11,17,23)]
  names(data)[1] = "hospital"
  names(data)[2] ="state"
  names(data)[3]="heart attack"
  names(data)[4]="heart failure"
  names(data)[5]="pneumonia"
  
  # validate state
  
  states <- unique(data[,2])
  if( state %in% states == FALSE) stop ("invalid state")
  
  # best hospital 
  
  data <- data[complete.cases(data),]
  data <- data[data$state==state,]
    vals      <- data[,outcome]   
  rowNum <- which.min(vals)
  ## Return hospital name in that state with lowest 30-day death rate
  data[rowNum, ]$hospital
}