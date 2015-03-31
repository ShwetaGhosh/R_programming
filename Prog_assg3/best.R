best <- function(state, outcome) {
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
  
  #remove NAs
  proper_data = function (df){
          df[complete.cases(df),]
        }
  # best hospital 
  least_rate = function(df)
  { row_num <- which.min(df[,outcome])
  ## Return hospital name in that state with lowest 30-day death rate
  df[row_num, ]$hospital
  }
  
   data_list <- split(data, data$state)
   data_list <-lapply(data_list,proper_data)
   vec_c <- lapply(data_list,least_rate)
   #vec_c1 <- do.call(rbind.data.frame, vec_c)
  #names(vec_c) <- c("state", "hospital_num")
  
  #data_list[vec_c$state,]$hospital
  #c<- as.numeric(vec_c$state)
  #data[c,]$hospital
  vec_c[state]
  #summary(data_list$state)
  
  
  # rowNum <- which.min(vals)
  ## Return hospital name in that state with lowest 30-day death rate
#  data[rowNum, ]$hospital
}