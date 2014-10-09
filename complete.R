complete <- function(directory, id = 1:332) {
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
  
  df_output <- data.frame()
  
  for(i in id){
    file_id <- sprintf("%03d", i)
    file_name <- paste(directory,'/', file_id, '.csv', sep='')
    df_input <- read.csv(file_name, header=TRUE, stringsAsFactors=TRUE)
    nobs_complete <- nrow(df_input[complete.cases(df_input), ])
    df_current <- c(i, nobs_complete)
    df_output <- rbind(df_output, df_current)
  }
  
  colnames(df_output) <- c('id', 'nobs')
  return(df_output)
}
