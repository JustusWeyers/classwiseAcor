#' acor_r
#' 
#' A function to actually calculate the autocorrelation classwise via c++
#'  
#' @param data a timeseries data.frame with a timestamp column in first position
#' @param acf A prepared data.frame to fill results in
#' 
#' @return A data.frame with class-wise levels of autocorrelation
#' 

acor_cpp <- function(data, acf, stop) {
  # Loop over rows in result data.frame
  for (i in 1:nrow(acf)) {
    # Calculate pairs via Rcpp function
    pairs = acor_pairs(data = data, FromTo = c(acf[i,1], acf[i,2]))
    # print(head(data.frame(x1 = pairs[,1], x2 = pairs[,2])[order(pairs[,1]),], n = 30))
    # Calculate correlation coefficient
    acf[i,3] = cor(pairs[,1], pairs[,2])
    # Count number of instances
    acf[i,4] = nrow(pairs)
    # Eventually kill process
    if (acf[i,3] < stop) break
  }
  print("acor_cpp()")
  print(na.omit(acf))
  # Return NA-free result table
  return(na.omit(acf))
}
