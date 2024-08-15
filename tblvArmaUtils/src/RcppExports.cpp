// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// rcpp_mc_log_mlike
arma::vec rcpp_mc_log_mlike(int N, arma::mat x, arma::mat alpha, arma::mat beta, arma::mat kappa, double phi, double sigma, Rcpp::List E_post_sample_list);
RcppExport SEXP _tblvArmaUtils_rcpp_mc_log_mlike(SEXP NSEXP, SEXP xSEXP, SEXP alphaSEXP, SEXP betaSEXP, SEXP kappaSEXP, SEXP phiSEXP, SEXP sigmaSEXP, SEXP E_post_sample_listSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type kappa(kappaSEXP);
    Rcpp::traits::input_parameter< double >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type E_post_sample_list(E_post_sample_listSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_mc_log_mlike(N, x, alpha, beta, kappa, phi, sigma, E_post_sample_list));
    return rcpp_result_gen;
END_RCPP
}
// LSE
double LSE(arma::vec x);
RcppExport SEXP _tblvArmaUtils_LSE(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(LSE(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_tblvArmaUtils_rcpp_mc_log_mlike", (DL_FUNC) &_tblvArmaUtils_rcpp_mc_log_mlike, 8},
    {"_tblvArmaUtils_LSE", (DL_FUNC) &_tblvArmaUtils_LSE, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_tblvArmaUtils(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
