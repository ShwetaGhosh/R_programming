corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  files_list<-list.files(directory,pattern="*.csv",full.names=TRUE)
  t<- complete(directory)
  r<- subset(t[,1],t[,2]> threshold)
  cov_data <- data.frame()
  
  final_cov <- c(NULL)
  for (i in 1:length(r))
    if (length(r)>0)
    { cov_data <- read.csv(files_list[r[i]])
                           cov_data <-cov_data[complete.cases(cov_data),]
      cor_data<-cor(cov_data$sulfate,cov_data$nitrate)
      final_cov <-c(final_cov,cor_data)}
            
    final_cov
  }