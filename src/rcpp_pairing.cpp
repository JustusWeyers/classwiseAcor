#include <Rcpp.h>

//' Creates pairs of data
//' 
//' @name acor_pairs
//'
//' @param data A data.frame. First column: date, second column: values.
//' @param FromTo Limits for pairing
//'
//' @return Two column matrix of paired data

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix acor_pairs (NumericMatrix data, NumericVector FromTo) {
  // Number of maximum rows of result data.frame
  int n = data.rows() * (FromTo[1] - FromTo[0]);
  // Empty matrix for results with n rows and two columns
  NumericMatrix values(n, 2);
  // Keep track of next row to store result
  int runner = 0;

  // Iterate over each row in 'data'
  for (int i = 0; i < data.rows(); i++) {
    // For each row in data set j to 0
    int j = 0;
    // For each timestamp get the timestamps within by FromTo
    while (i - j >= 0 && j < FromTo[1]) {
      if ((data(i, 0) - data(i-j, 0)) >= FromTo[0] && (data(i, 0) - data(i-j, 0)) < FromTo[1]) {
        // Store value of current timestamp in first column
        values(runner, 0) = data(i, 1);
        // Store value of FromTo-data in second column
        values(runner, 1) = data(i-j, 1);
        // Increment global runner variable
        runner++;
      }
      // Increment j for the next FromTo-data
      j++;
    }
  }

  // NumericMatrix 'values' contains empty rows. The next loop serves to
  // remove them. Therefore an empty matrix of 'runner' rows and two
  // columns is created
  NumericMatrix values_clean(runner,2);

  // Iterate over rows of 'values_clean'
  for (int i = 0; i < values_clean.rows(); i++) {
    // Copy results to clean matrix
    values_clean(i,_) = values(i,_);
  }

  // Return clean matrix
  return values_clean;
}