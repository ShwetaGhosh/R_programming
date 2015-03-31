cocomplete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  files_list<-list.files(directory,pattern="*.csv",full.names=TRUE)
  nobs <- data.frame()
  final <- data.frame()
    for (i in id) { nobs <- read.csv(files_list[i])
      nobs <- sum(complete.cases(nobs))
      final <- rbind(final,cbind(i,nobs))}
      final
  #d <- data.frame()
  #for (i in 1:20) {d <- rbind(d,c(i+i, i*i, i/1))}
  #final
      #dat <- cbind(dat,read.csv(files_list[i]),make.row.names=TRUE)}
  #dat
}