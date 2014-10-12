rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  input <- read.csv('outcome-of-care-measures.csv', header=TRUE, colClasses = 'character')
  
  cols <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
  
  if (nrow(subset(input, State == state)) == 0){
    stop('invalid state')
  }
  
  if (outcome %in% names(cols) == FALSE){
    stop('invalid outcome')
  }
  
  if (!is.numeric(num) && !(num %in% c('best', 'worst'))){
    stop('invalid num')
  }
  
  outcome_index <- cols[[outcome]]
  
  sample <- input[,c(2,7,outcome_index)]
  sample[,3] <- as.numeric(sample[,3])
  
  colnames(sample) <- c('Hospital', 'State', 'Outcome')
  
  sample <- subset(sample, State == state)
  sample <- subset(sample, !is.na(Outcome))
  sample <- sample[with(sample, order(sample[,3], sample[,1])), ]
  
  nobs <- nrow(sample)
  
  if (is.numeric(num)){
    if(num > nobs){
      return(NA)
    } else {
      return(sample[[num, 1]])
    }
  } else {
    if(num == 'best'){
      return(sample[[1,1]])
    } else if(num == 'worst'){
      return(sample[[nobs,1]])
    }
  }

}