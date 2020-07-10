// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
using namespace Rcpp;

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]


// via the exports attribute we tell Rcpp to make this function
// available from R
//
// [[Rcpp::export]]
arma::mat rmvrnorm_arma(int x, arma::mat Eigen_matrix) {
  // sample observations
  NumericVector v = rnorm(2 * x);
  v.attr("dim") = Dimension(x, 2);

  arma::mat out = as<arma::mat>(v);

  return Eigen_matrix * out.t();
}
// #include <RcppArmadillo.h>
// using namespace Rcpp; using namespace arma;
//

//

