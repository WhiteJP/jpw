#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector censor_right_c(NumericVector x, double max) {
  int n = x.size();
  NumericVector out(n);
  for(int i = 0; i < n; ++i) {
    if (x[i] > max) {
      out[i] = max;
    } else {
      out[i] = x[i];
    }
  }
  return out;
}
