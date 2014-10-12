rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name


  input <- read.csv('outcome-of-care-measures.csv', header=TRUE, colClasses = 'character')
  
  cols <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
  
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
  
  rankall_hospital <- function(state){
  
    group <- subset(sample, State == state)
    group <- subset(group, !is.na(Outcome))
    group <- group[with(group, order(group[,3], group[,1])), ]
    
    nobs <- nrow(group)
    
    if (is.numeric(num)){
      if(num > nobs){
        return(NA)
      } else {
        return(group[[num, 1]])
      }
    } else {
      if(num == 'best'){
        return(group[[1,1]])
      } else if(num == 'worst'){
        return(group[[nobs,1]])
      }
    }
    
  }
  
  states <- sort(unique(sample$State))
  hospitals <- lapply(states, rankall_hospital)
  
  result <- data.frame(unlist(hospitals), states)
  colnames(result) <- c('hospital', 'state')
  
  return(result)

}