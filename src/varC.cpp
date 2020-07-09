#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double varC(NumericVector x, double mean) {
  int n = x.size();
  double res = 0;
  for (int i=0; i<n; ++i){
    res += pow(x[i] - mean, 2);
  }
  return res / (n - 1);
}


