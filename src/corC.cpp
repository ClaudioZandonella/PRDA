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
  double mx = 0;
  double my = 0;
  for (int i = 0; i < n; i++){
    mx += x[i];
    my += y[i];
  }
  mx /= n;
  my /= n;

  // compute cov and sds
  double cor = 0;
  double sdx = 0;
  double sdy = 0;
  double xd = 0;
  double yd = 0;
  for (int i = 0; i < n; i++){
    xd = x[i] - mx;
    yd = y[i] - my;
    cor += xd * yd;
    sdx += pow(xd, 2);
    sdy += pow(yd, 2);
  }

  cor /= n1;
  sdx /= n1;
  sdy /= n1;
  cor /= (sqrt(sdx) * sqrt(sdy));

  return cor;
}
