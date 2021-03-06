// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// cohen_loop
List cohen_loop(double sample_n1, double mean_diff, double sample_n2, String test_method, String alternative, double ratio_sd, double mu, int B);
RcppExport SEXP _PRDA_cohen_loop(SEXP sample_n1SEXP, SEXP mean_diffSEXP, SEXP sample_n2SEXP, SEXP test_methodSEXP, SEXP alternativeSEXP, SEXP ratio_sdSEXP, SEXP muSEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type sample_n1(sample_n1SEXP);
    Rcpp::traits::input_parameter< double >::type mean_diff(mean_diffSEXP);
    Rcpp::traits::input_parameter< double >::type sample_n2(sample_n2SEXP);
    Rcpp::traits::input_parameter< String >::type test_method(test_methodSEXP);
    Rcpp::traits::input_parameter< String >::type alternative(alternativeSEXP);
    Rcpp::traits::input_parameter< double >::type ratio_sd(ratio_sdSEXP);
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< int >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(cohen_loop(sample_n1, mean_diff, sample_n2, test_method, alternative, ratio_sd, mu, B));
    return rcpp_result_gen;
END_RCPP
}
// cor_loop
List cor_loop(int n, String alternative, int B, arma::mat Eigen_matrix);
RcppExport SEXP _PRDA_cor_loop(SEXP nSEXP, SEXP alternativeSEXP, SEXP BSEXP, SEXP Eigen_matrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< String >::type alternative(alternativeSEXP);
    Rcpp::traits::input_parameter< int >::type B(BSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Eigen_matrix(Eigen_matrixSEXP);
    rcpp_result_gen = Rcpp::wrap(cor_loop(n, alternative, B, Eigen_matrix));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_PRDA_cohen_loop", (DL_FUNC) &_PRDA_cohen_loop, 8},
    {"_PRDA_cor_loop", (DL_FUNC) &_PRDA_cor_loop, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_PRDA(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
