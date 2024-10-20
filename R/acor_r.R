#' acor_r
#' 
#' A function to actually calculate the autocorrelation classwise via R
#'
#' @param data A timeseries data.frame with a timestamp column in first position
#' @param acf A prepared data.frame to fill results in
#' @param stop Stop calculation when reached a certain minimum

#' @return A data.frame with class-wise levels of autocorrelation



acor_r <- function(data, acf, stop){
  # Calculate distance matrix
  dist_data <- as.matrix(stats::dist(data[,1], method = "euclidean", p = 2))
  # Delete upper triangle
  dist_data[upper.tri(dist_data, diag = TRUE)] <- NA
  # Loop over classes aka rows in acf
  for(i in 1:(nrow(acf)-1)) {
    sel = which(dist_data >= acf[i,1] & dist_data < acf[i,2], arr.ind = TRUE)
    # print(head(data.frame(x1 = data[sel[,1],2], x2 = data[sel[,2],2])[order(data[sel[,1],2]),], n = 30))
    acf[i,3] <- stats::cor(data[sel[,1],2], data[sel[,2],2])
    acf[i,4] <- length(sel)
    # Eventually kill process
    if (acf[i,3] < stop) break
  }
  # Return NA-free result table
  return(stats::na.omit(acf))
}
