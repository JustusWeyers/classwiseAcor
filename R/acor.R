#' acor
#' 
#' Calculate the autocorrelation classwise for a timeseries data.frame 
#' 
#' @param data A timeseries data.frame with a timestamp column in first position
#' @param dt  Class-width in days
#' @param method Choose between calculation via "r" or "cpp"
#' @param stop Stop calculation when reached a certain minimum
#' 
#' @return A data.frame with class-wise levels of autocorrelation
#' 
#' @export
#'

acor = function(data, method = "r", dt = 30, stop = 0.5) {
  # Remove NA's from data
  data = stats::na.omit(data)
  # Turn timestamp into numeric
  data[,1] = as.numeric(data[,1])
  # Turn data into unnamed matrix
  data = unname(as.matrix(data))
  
  # Class definitions
  classes = c(1, seq(dt/2, to = floor(0.25 * diff(range(data[,1]))), by = dt))
  
  # Setup result table
  acf <- as.data.frame(matrix(ncol = 4, nrow = (length(classes)-1)))
  names(acf) <- c("From", "To", "Autocorrelation", "Number.of.instances")
  # Fill column 'From'
  acf$From = classes[-length(classes)]
  # Fill column 'To'
  acf$To = classes[-1]
  
  if (identical(method, "r")) {
    return(acor_r(data, acf, stop))
  } else if (identical(method, "cpp")) {
    return(acor_cpp(data, acf, stop))
  } else {
    return(NULL)
  }
  
}
