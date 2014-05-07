# rankhospital

rankhospital <- function(ST, outcome, num){
  outcomefn <- "outcome-of-care-measures.csv"
  dat <<- read.csv(outcomefn, colClasses = "character", na.strings = ("Not Available"))
  mydf <- cbind(dat$State, dat$Hospital.Name, dat[11], dat[17],dat[23])
  names(mydf) <- c("state", "HN", "heart attack", "heart failure", "pneumonia")
  if(ST %in% mydf$state == FALSE) stop("invalid state")
  if(outcome %in% colnames(mydf) == FALSE) stop("invalid outcome")
  if(num=="best") num<-1
  if(num=="worst"){
    l <- length(sortdf[,1])
    num <- l    
  } 
  smalldf <- mydf[mydf$state==ST,]
  compdf <- smalldf[complete.cases(smalldf),]
  sortdf <- compdf[ order(as.numeric(compdf[,outcome])),]
#   print(head(sortdf))
   return(as.character(sortdf[num,]$HN))
  
  
#   (as.numeric(compdf[,outcome]))
  
  
#source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
# knu94EhPZt
#   bestlist <- compdf$HN[compdf[,outcome]==minval]
#   besthosp <- as.character(sort(bestlist)[1])
}
  
fifth <- rankhospital("CO", "heart failure", 5)
# print(fifth)
best <- rankhospital("CO", "pneumonia", "best")
# print(best)
worst <- rankhospital("NC", "pneumonia", "worst")
# print(worst)