
hospfn <-  "/Users/dave/Google Drive/dev/R/coursera/rprog-002/ProgrammingAssignment3/rprog_data_ProgAssignment3-data/hospital-data.csv"
outcomefn <-  "/Users/dave/Google Drive/dev/R/coursera/rprog-002/ProgrammingAssignment3/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv"

best <- function(ST, outcome){
  outcomefn <- "outcome-of-care-measures.csv"
  dat <<- read.csv(outcomefn, colClasses = "character", na.strings = ("Not Available"))
  mydf <- cbind(dat$State, dat$Hospital.Name, dat[13], dat[19],dat[25])
  names(mydf) <- c("state", "HN", "heart attack", "heart failure", "pneumonia")
  if(ST %in% mydf$state == FALSE) stop("invalid state")
  if(outcome %in% colnames(mydf) == FALSE) stop("invalid outcome")
  smalldf <- mydf[mydf$state==ST,]
  compdf <- smalldf[complete.cases(smalldf),]
  minval <- min(as.numeric(compdf[,outcome]))
  bestlist <- compdf$HN[compdf[,outcome]==minval]
  besthosp <- as.character(sort(bestlist)[1])
  return(besthosp)
}

#source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
# knu94EhPZt
ans <- best("NY", "pneumonia")
# dat$State
# dat$Hospital.Name
# dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
# dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
# dat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
# dat$Hospital.Name[dat$Hospital.30.Day.Death..Mortality..Rates..from.Heart.Failure==min(dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) 
# best("TX", "heart attack")
# # [1] "CYPRESS FAIRBANKS MEDICAL CENTER";
# best("TX", "heart failure")
# # [1] "FORT DUNCAN MEDICAL CENTER";
# best("MD", "heart attack")
# # [1] "JOHNS HOPKINS HOSPITAL, THE";
# best("MD", "pneumonia")
# # [1] "GREATER BALTIMORE MEDICAL CENTER";p
# # best("BB", "heart attack")
# # # Error in best("BB", "heart attack") : invalid state;
# # best("NY", "hert attack")
# # # Error in best("NY", "hert attack") : invalid outcome;
# 
# 
