#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector censor_left_c(NumericVector x, double min) {
  int n = x.size();
  NumericVector out(n);
  for(int i = 0; i < n; ++i) {
    if (x[i] < min) {
      out[i] = min;
    } else {
      out[i] = x[i];
    }
  }
  return out;
}

