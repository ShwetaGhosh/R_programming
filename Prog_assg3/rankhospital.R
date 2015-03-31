rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv" ,colClasses = "character")
  #change_numeric <- function (df){ df[,x] <- as.numeric(df[,x])}
  data <- data[c(2,7,11,17,23)]
  
  # converting to outcome values to type numeric
  suppressWarnings(data[, c(3:5)] <- sapply(data[,c(3:5)],as.numeric))
  
  # validate outcome
  outcomes = c("heart attack" , "heart failure" , "pneumonia")
  if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
  
  columns = c("hospital","state","heart attack", "heart failure", "pneumonia")
  names(data) <- columns
  
  # validate state
  
  states <- unique(data[,2])
  if( state %in% states == FALSE) stop ("invalid state")
  
  ## Validate the num value
  if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
  
  #remove NAs
  proper_data = function (df){
    df[complete.cases(df),]
  }
  
  
  # order hospital 
  order_data = function (df){ 
    #df[sapply(df,order(df$hospital, df[outcome]),na.last=TRUE,decreasing=FALSE),]
  df[order(df[outcome],df$hospital, na.last=TRUE,decreasing = FALSE), ]
 # df[order(df[outcome], na.last=TRUE,decreasing = FALSE), ]
  #df
  }
  
  rank = function (df) { 
  vals<- df[,outcome]   
  ## Return hospital name in that state with the given rank
  if (num == "best")
  {rowNum <- which.min(vals)}
  if (num == "worst")
  {rowNum <- which.max(vals)}
  else {rowNum <- num}
  ## Return hospital name in that state with the given rank
  df[rowNum, ]$hospital
}

data_list <- split(data, data$state)
data_list <-lapply(data_list,proper_data)
data_list <- lapply(data_list,order_data)
vec_2   <-  lapply(data_list,rank)
vec_2[state]
}