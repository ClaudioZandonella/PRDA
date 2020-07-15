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
List cohen_loop(double sample_n1, double effect_target, double sample_n2,
                  String test_method, String alternative, double mu, int B) {

  NumericVector estimate(B);
  NumericVector pval(B);
  double df = 0;
  double mx = 0;
  double vx = 0;
  double my = 0;
  double vy = 0;
  double v = 0;
  double stderr = 0;
  double stderrx_2 = 0;
  double stderry_2 = 0;
  double tstat = 0;

  for(int i = 0; i < B; i++){
    // sample obs
    NumericVector x = rnorm(sample_n1, effect_target, 1);
    NumericVector y = 0;
    if(sample_n2 != 0){
      y = rnorm(sample_n2, 0, 1);
      }

    // compute test statisttic
    if (test_method == "one_sample") {
      df = sample_n1 - 1;
      mx = mean(x);
      vx = var(x);
      stderr = sqrt(vx / sample_n1);
      tstat = (mx - mu) / stderr;
      estimate[i] = (mx - mu) / sd(x);
    } else if (test_method == "paired"){
      df = sample_n1-1;
      NumericVector diff = x - y;
      double mdiff = mean(diff);
      double vdiff = var(diff);
      stderr = sqrt(vdiff / sample_n1);
      tstat = (mdiff - mu) / stderr;
      estimate[i] = (sample_n1-2)/(sample_n1-1.25) * mdiff / sd(diff);
    } else if (test_method == "two_samples") {
      mx = mean(x);
      vx = var(x);
      df = sample_n1+sample_n2-2;
      my = mean(y);
      vy = var(y);
      v = (sample_n1-1)*vx + (sample_n2-1)*vy;
      v = v/df;
      stderr = sqrt(v*(1/sample_n1 + 1/sample_n2));
      tstat = (mx - my - mu)/stderr;
      estimate[i] = (1 - (3/((4*df)-1))) * (mx-my)/sqrt(v);
    } else {
      mx = mean(x);
      vx = var(x);
      my = mean(y);
      vy = var(y);
      stderrx_2 = vx/sample_n1;
      stderry_2 = vy/sample_n2;
      stderr = sqrt(stderrx_2 + stderry_2);
      df = pow(stderr, 4)/(pow(stderrx_2, 2)/(sample_n1-1) + pow(stderry_2, 2)/(sample_n2-1));
      tstat = (mx - my - mu)/stderr;
      estimate[i] = (mx-my)/sqrt((vx + vy)/2);
    }

    // compute pval
    if (alternative == "two.sided"){
      pval[i] = 2*(Rf_pt(std::abs(tstat), df, false, false));
    } else if (alternative == "greater") {
      pval[i] = Rf_pt(tstat, df, false, false);
    } else {
      pval[i] = 1-Rf_pt(tstat, df, false, false);
    }
  }

  return List::create(Named("p.value") = pval,
                      Named("estimate") = estimate);
}
