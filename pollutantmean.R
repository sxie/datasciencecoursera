pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
  col_SUMS = 0
  col_CNTS = 0
  for (i in id){
    padded_id <- sprintf("%03d.csv",i)
    dat <- read.csv(paste(getwd(),"/",directory,"/",padded_id,sep=""))
    col_sum <- colSums(dat[pollutant], na.rm = TRUE)
    col_cnt <- sum(!is.na(dat[pollutant]))
    col_SUMS <- col_SUMS + col_sum
    col_CNTS <- col_CNTS + col_cnt
  }
  #return mean
  calcMean <- col_SUMS/col_CNTS
  round(calcMean[[pollutant]], digits = 3)
}
