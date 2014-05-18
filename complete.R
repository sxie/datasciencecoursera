complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  index <- 1
  nobsvector <- integer(length(id))
  for (i in id){
    padded_id <- sprintf("%03d.csv",i)
    dat <- read.csv(paste(getwd(),"/",directory,"/",padded_id,sep=""))
    nobs <- sum(complete.cases(dat))
    nobsvector[index] <- nobs
    index <- index + 1
  }
  x <- data.frame(id=id, nobs=nobsvector)
  x
}
