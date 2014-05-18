corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  ## Return a numeric vector of correlations
  cors = numeric()
  for (i in 1:332){
    padded_id <- sprintf("%03d.csv",i)
    dat <- read.csv(paste(getwd(),"/",directory,"/",padded_id,sep=""))
    datcomplete <- subset(dat,dat$sulfate != "NA" & dat$nitrate != "NA")
    thcheck <- length(datcomplete$ID)
    if (thcheck > threshold){
      cors <- c(cors,cor(datcomplete$sulfate, datcomplete$nitrate))
    }
  }
  print(cors)
}
