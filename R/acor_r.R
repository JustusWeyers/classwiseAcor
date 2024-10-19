#' acor_r
#' 
#' A function to actually calculate the autocorrelation classwise via R
#'
#' @param data A timeseries data.frame with a timestamp column in first position
#' @param acf A prepared data.frame to fill results in
#' 
#' @return A data.frame with class-wise levels of autocorrelation
#' 

acor_r <- function(data, acf){
  # Calculate distance matrix
  dist_data <- as.matrix(dist(data[,1], method = "euclidean", p=2))
  # Delee upper triangle
  dist_data[upper.tri(dist_data, diag = TRUE)] <- NA
  # Loop over classes aka rows in acf
  for(i in 1:(nrow(acf)-1)) {
    sel = which(dist_data >= acf[i,1] & dist_data < acf[i,2], arr.ind = TRUE)
    acf[i,3] <- cor(data[sel[,1],2], data[sel[,2],2])
    acf[i,4] <- length(sel)
  }
  # Return NA-free result table
  return(na.omit(acf))
}
