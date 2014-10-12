best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  input <- read.csv('outcome-of-care-measures.csv', header=TRUE, colClasses = 'character')
  
  cols <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)

  if (nrow(subset(input, State == state)) == 0){
    stop('invalid state')
  }
  
  if (outcome %in% names(cols) == FALSE){
    stop('invalid outcome')
  }
  
  outcome_index <- cols[[outcome]]
  
  sample <- subset(input, State == state)
  sample <- sample[,c(2,outcome_index)]
  sample[,2] <- as.numeric(sample[,2])
  sample <- sample[with(sample, order(sample[,2], sample[,1])), ]
  
  thebest <- sample[[1,1]]

  return(thebest)
  
}