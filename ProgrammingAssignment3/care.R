
hospfn <-  "/Users/dave/Google Drive/dev/R/coursera/rprog-002/ProgrammingAssignment3/rprog_data_ProgAssignment3-data/hospital-data.csv"
outcomefn <-  "/Users/dave/Google Drive/dev/R/coursera/rprog-002/ProgrammingAssignment3/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv"


#care <- read.csv(caremeas, colClasses = "character")
outcome <- read.csv(outcomefn, colClasses = "character")
# PART 1
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11]) # pretty.

# PART 2
best <- function(ST, illness){
  outcomefn <-  "/Users/dave/Google Drive/dev/R/coursera/rprog-002/ProgrammingAssignment3/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv"
  dat <- read.csv(outcomefn, colClasses = "character")
  lowest <- min(dat$illness)
  print(lowest)
}
print("==========================")
print("Best Test = ")
print(best("CO", "heart attack"))
