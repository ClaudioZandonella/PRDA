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
double corC(NumericVector x, NumericVector y) {
  int n = x.size();
  int n1 = n - 1;

  // compute means
  double mx = mean(x);
  double my = mean(y);

  // compute cov and sds
  double cor = 0;
  double sdx = sd(x);
  double sdy = sd(y);
  for (int i = 0; i < n; i++){
    cor += (x[i] - mx) * (y[i] - my);
  }

  cor /= n1;
  cor /= (sdx * sdy);

  return cor;
}
