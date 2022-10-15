#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector censor_both_c(NumericVector x, double min, double max) {
  int n = x.size();
  NumericVector out(n);
  for(int i = 0; i < n; ++i) {
    if (x[i] > max) {
      out[i] = max;
    } else if (x[i] < min) {
      out[i] = min;
    } else {
      out[i] = x[i];
    }
  }
  return out;
}

