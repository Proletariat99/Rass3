#rankall
# Takes a num and an outcome and returns the hospital name that is ranked that num for each state
# 


rankall <- function(outcome, num){
  outcomefn <- "outcome-of-care-measures.csv"
  dat <<- read.csv(outcomefn, colClasses = "character", na.strings = ("Not Available"))
  mydf <- cbind(dat$State, dat$Hospital.Name, dat[11], dat[17],dat[23])
  names(mydf) <- c("state", "HN", "heart attack", "heart failure", "pneumonia")
  if(outcome %in% colnames(mydf) == FALSE) stop("invalid outcome")
  if(num=="best") num<-1
  mydf$"heart attack" <- as.numeric(mydf$"heart attack")
  mydf$"heart failure" <- as.numeric(mydf$"heart failure")
  mydf$pneumonia <- as.numeric(mydf$pneumonia)
#   d <- mydf
#   sortdf <- d[ order(d$state, d$pneumonia), ]
  x <- mydf[,outcome]
  f <- factor(mydf$state)
  states <- tapply(x, f, sort)
  z <- vector(mode="numeric", length=0)
  for(state in states){
    z <- append(z, state[num])
    h <- append(h, )
  }
  result <- data.frame()
  result$state <- states
  result$hospital <- mydf$hospital[mydf[outcome,]==z]
#   names(result) <- c('hospital', 'state')
  return(z)

}


ans <- rankall("heart attack", 20)


#   if(num=="worst"){
#     l <- length(mydf[,1])
#     num <- l    
#   }