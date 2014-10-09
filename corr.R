corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  files <- list.files(directory)
  vect_output = c()
  
  for(i in 1:length(files)){
    
    file_name <- paste(directory,'/', files[i], sep='')
    df_input <- read.csv(file_name, header=TRUE, stringsAsFactors=TRUE)
    df_input_complete <- df_input[complete.cases(df_input), ]
    nobs_complete <- nrow(df_input_complete)
    
    correl <- cor(df_input_complete$sulfate, df_input_complete$nitrate)
    
    if (nobs_complete >= threshold){
      vect_output <- c(vect_output, correl)
    }
    
  }
  
  return(vect_output)
  
}
