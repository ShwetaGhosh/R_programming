rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv" ,colClasses = "character")
  suppressWarnings(data[, 11] <- as.numeric(data[, 11]))
  suppressWarnings(data[, 17] <- as.numeric(data[, 17]))
  suppressWarnings(data[, 23] <- as.numeric(data[, 23]))
  
  ## Check that state and outcome are valid
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
  
      
  ## Validate the num value
  if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
  
  ## For each state, find the hospital of the given rank
  data <- data[complete.cases(data),]
  #data1 <- split (data,data$state)
  
  #lapply (data1,function (x)order(x[]) )
  
  
  data <- data[order(data$hospital, decreasing = FALSE), ]
  data <- data[order(data[outcome], decreasing = FALSE), ]
  
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  getHospByRank <- function(df, s, n) {
    df <- df[df$state==s, ]
    vals <- df[, outcome]
     if( n == "best" ) 
       {
      rowNum <- which.min(vals)
       } 
    else if( n == "worst" ) 
      {  rowNum <- which.max(vals)
         } 
    else {
          rowNum <- n
         }
    df[rowNum, ]$hospital
     }
  
     ## For each state, find the hospital of the given rank
  # validate state
  states <- sort(unique(data[,2]))
  #if( state %in% states == FALSE) stop ("invalid state")
  
    newdata <- data.frame("hospital"=character(), "state"=character(), stringsAsFactors =FALSE)
  for(st in states) {
    hosp <- getHospByRank(data, st, num)
    newdata <- rbind(newdata, data.frame(hospital=hosp, state=st))
     }
  
     ## Return a data frame with the hospital names and the (abbreviated) state name
     newdata <- newdata[order(newdata$state, decreasing = FALSE), ]
     newdata
  
}