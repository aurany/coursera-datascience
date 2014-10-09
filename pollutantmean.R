pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  df_total <- data.frame()
  
  for(i in id){
    file_id <- sprintf("%03d", i)
    file_name <- paste(directory,'/', file_id, '.csv', sep='')
    df_current <- read.csv(file_name, header=TRUE, stringsAsFactors=TRUE)
    df_total <- rbind(df_total, df_current)
  }
  
  mean <- mean(df_total[,pollutant], na.rm=TRUE)
  mean_fmt <- sprintf("%.3f", mean)
  
  return(mean_fmt)
    
}