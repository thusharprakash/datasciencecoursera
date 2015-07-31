best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
  tmp <-split(data,data$State)
  
  ## Check that state and outcome are valid
  if(sum(state == data$State) == 0){
  
    stop("invalid state")
  }
  if(outcome == "heart attack" ){
    ind <- 11
    
  }
  else if(outcome == "heart failure"){
    ind <- 17
  }
  else if(outcome == "pneumonia"){
    ind <- 23
  }
  else{
    stop("invalid outcome")
  }
  
 
  rframe <- as.data.frame(tmp[state])
  minDat <- tapply(as.numeric(rframe[,ind]),rframe[2],min)

  
  
  avail <- minDat[!is.na(minDat)]
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  minimum <- min(avail)
  ns <- names(avail[avail == min(avail)])
  ns <- sort(ns)
  ns[1]
  
}