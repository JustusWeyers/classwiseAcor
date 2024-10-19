#' acor_r
#' 
#' A function to actually calculate the autocorrelation classwise via c++
#'  
#' @param data a timeseries data.frame with a timestamp column in first position
#' @param acf A prepared data.frame to fill results in
#' 
#' @return A data.frame with class-wise levels of autocorrelation
#' 

acor_cpp <- function(data, acf) {
  # Loop over rows in result data.frame
  for (i in 1:nrow(acf)) {
    # Calculate pairs via Rcpp function
    pairs = acor_pairs(data = data, FromTo = c(acf[i,1], acf[i,2]))
    # Calculate correlation coefficient
    acf[i,3] = cor(pairs)[1,2]
    # Count number of instances
    acf[i,4] = nrow(pairs)
  }
  # Return NA-free result table
  return(na.omit(acf))
}
