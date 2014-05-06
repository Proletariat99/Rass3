
hospfn <-  "/Users/dave/Google Drive/dev/R/coursera/rprog-002/ProgrammingAssignment3/rprog_data_ProgAssignment3-data/hospital-data.csv"
outcomefn <-  "/Users/dave/Google Drive/dev/R/coursera/rprog-002/ProgrammingAssignment3/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv"


#care <- read.csv(caremeas, colClasses = "character")
# outcome <- read.csv(outcomefn, colClasses = "character")
# PART 1
# outcome[,11] <- as.numeric(outcome[,11])
# hist(outcome[,11]) # pretty.


# PART 2
best <- function(ST, outcome){
  outcomefn <-  "/Users/dave/Google Drive/dev/R/coursera/rprog-002/ProgrammingAssignment3/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv"
  dat <<- read.csv(outcomefn, colClasses = "character", na.strings = ("Not Available"))
  mydf <- cbind(dat$State, dat$Hospital.Name, dat[13], dat[19],dat[25])
  names(mydf) <- c("state", "HN", "heart attack", "heart failure", "pneumonia")
  if(ST %in% mydf$state == FALSE) stop("invalid state")
  if(outcome %in% colnames(mydf) == FALSE) stop("invalid outcome")
  smalldf <- mydf[mydf$state==ST,]
  minval <- min(as.numeric(smalldf[,outcome]), na.rm=TRUE)
#   smalldf[smalldf[,outcome] == minval]
  Best_Hospital <- as.character(smalldf$HN[na.omit(smalldf[,outcome] == minval)])
#   HAminval <- apply(df[11], 2, min)
#   HFminval <- apply(df[17], 2, min)
#   PNminval <- apply(df[23], 2, min)
  return(sort(Best_Hospital)[1])
}


#ans <- best("TX", "heart attack")
# print(colnames(dat)[1:24])
# heart attack = [11]
# heart failure = [17]
# pneumonia = [23]

# dat$State
# dat$Hospital.Name
# dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
# dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
# dat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
# dat$Hospital.Name[dat$Hospital.30.Day.Death..Mortality..Rates..from.Heart.Failure==min(dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)]


# 
# best("TX", "heart attack")
# # [1] "CYPRESS FAIRBANKS MEDICAL CENTER";
# best("TX", "heart failure")
# # [1] "FORT DUNCAN MEDICAL CENTER";
# best("MD", "heart attack")
# # [1] "JOHNS HOPKINS HOSPITAL, THE";
# best("MD", "pneumonia")
# # [1] "GREATER BALTIMORE MEDICAL CENTER";p
# best("BB", "heart attack")
# # Error in best("BB", "heart attack") : invalid state;
# best("NY", "hert attack")
# # Error in best("NY", "hert attack") : invalid outcome;